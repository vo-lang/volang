use std::path::PathBuf;

use serde::{Deserialize, Serialize};
use vo_common::vfs::FileSystem;

use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::profile::ArtifactRole;
use crate::schema::lockfile::{LockedArtifactMode, LockedModule};
use crate::version::ExactVersion;
use crate::Error;

pub const MATERIALIZATION_ATTESTATION_FORMAT: u64 = 1;
pub const DETACHED_ARTIFACT_MANIFEST_FORMAT: u64 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum StaticInitializerPolicy {
    ProvenAbsent,
    CertifiedSideEffectFreeHostAdapter,
    IsolatedByWorkerOrProcess,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DetachedArtifactManifest {
    pub format: u64,
    pub module: ModulePath,
    pub version: ExactVersion,
    pub role: ArtifactRole,
    pub kind: String,
    pub name: String,
    pub target: String,
    pub capabilities: Vec<String>,
    pub schema: Digest,
    pub abi: Digest,
    pub content_digest: Digest,
    pub static_initializer_policy: StaticInitializerPolicy,
    pub factories: Vec<DetachedProviderFactory>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DetachedProviderFactory {
    pub factory_id: u32,
    pub role: ArtifactRole,
    pub export: String,
    pub abi: Digest,
    pub schema: Digest,
    pub capability_digest: Digest,
}

impl DetachedArtifactManifest {
    pub fn parse(bytes: &[u8]) -> Result<Self, Error> {
        if bytes.is_empty() || bytes.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::InvalidReleaseMetadata(format!(
                "detached artifact manifest must contain 1..={} bytes",
                vo_common::vfs::MAX_TEXT_FILE_BYTES,
            )));
        }
        serde_json::from_slice(bytes).map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "failed to parse detached artifact manifest: {error}"
            ))
        })
    }

    pub fn validate_against(
        &self,
        locked: &LockedModule,
        artifact: &AttestedRoleArtifact,
    ) -> Result<(), Error> {
        let selection = locked.selection.as_ref().ok_or_else(|| {
            Error::InvalidReleaseMetadata("locked module has no capability selection".to_string())
        })?;
        if self.format != DETACHED_ARTIFACT_MANIFEST_FORMAT
            || self.module != locked.path
            || self.version != locked.version
            || self.role != artifact.role
            || self.kind != artifact.kind
            || self.name != artifact.name
            || self.target != selection.target
            || self.capabilities != selection.capabilities
            || self.schema != selection.schema
            || self.abi != selection.abi
            || self.content_digest != artifact.content_digest
        {
            return Err(Error::InvalidReleaseMetadata(format!(
                "detached artifact manifest does not exactly match {}@{} role {}",
                locked.path,
                locked.version,
                artifact.role.as_str(),
            )));
        }
        if self.factories.is_empty() || self.factories.len() > crate::MAX_MODULE_METADATA_ENTRIES {
            return Err(Error::InvalidReleaseMetadata(
                "detached artifact manifest must declare a bounded factory table".to_string(),
            ));
        }
        let expected_capability_digest = crate::profile::CapabilitySet::normalize(
            &selection.capabilities,
            "locked capabilities",
        )?
        .digest();
        let mut identities = std::collections::BTreeSet::new();
        for factory in &self.factories {
            crate::profile::validate_stable_name(
                &factory.export,
                "detached artifact manifest factory export",
            )?;
            if factory.factory_id == 0
                || factory.role != artifact.role
                || factory.abi != selection.abi
                || factory.schema != selection.schema
                || factory.capability_digest != expected_capability_digest
                || !identities.insert((factory.factory_id, factory.export.as_str()))
            {
                return Err(Error::InvalidReleaseMetadata(format!(
                    "detached artifact manifest contains an invalid or duplicate factory for role {}",
                    artifact.role.as_str(),
                )));
            }
        }
        crate::identity::ArtifactId {
            kind: self.kind.clone(),
            target: self.target.clone(),
            name: self.name.clone(),
        }
        .validate()
        .map_err(Error::InvalidReleaseMetadata)
    }

    pub fn render(
        &self,
        locked: &LockedModule,
        artifact: &AttestedRoleArtifact,
    ) -> Result<Vec<u8>, Error> {
        self.validate_against(locked, artifact)?;
        let mut bytes = serde_json::to_vec_pretty(self).map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "failed to encode detached artifact manifest: {error}"
            ))
        })?;
        bytes.push(b'\n');
        Ok(bytes)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AttestedRoleArtifact {
    pub role: ArtifactRole,
    pub kind: String,
    pub name: String,
    pub size: u64,
    pub content_digest: Digest,
    pub detached_manifest_digest: Digest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaterializedSourceArtifact {
    pub role: ArtifactRole,
    pub id: crate::identity::ArtifactId,
    pub size: u64,
    pub digest: Digest,
    pub detached_manifest_digest: Digest,
    pub cache_relative_path: PathBuf,
    pub detached_manifest_relative_path: PathBuf,
}

#[derive(Debug)]
pub struct MaterializationOutput {
    pub role: ArtifactRole,
    pub kind: String,
    pub name: String,
    pub bytes: Vec<u8>,
    pub detached_manifest: Vec<u8>,
}

#[derive(Debug)]
pub struct PreparedMaterializedSourceArtifact {
    pub artifact: AttestedRoleArtifact,
    pub cache_relative_path: PathBuf,
    pub detached_manifest_relative_path: PathBuf,
    pub bytes: Vec<u8>,
    pub detached_manifest: Vec<u8>,
}

#[derive(Debug)]
pub struct PreparedDevMaterialization {
    pub attestation: DevMaterializationAttestation,
    pub attestation_cache_path: PathBuf,
    pub artifacts: Vec<PreparedMaterializedSourceArtifact>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DevMaterializationStatement {
    pub format: u64,
    pub module: ModulePath,
    pub version: ExactVersion,
    pub capabilities: Vec<String>,
    pub target: String,
    pub toolchain: String,
    pub source_recipe: Digest,
    pub schema: Digest,
    pub abi: Digest,
    pub vo_graph: Digest,
    pub rust_graph: Digest,
    pub js_graph: Digest,
    pub recipe_graph: Digest,
    pub inputs: Digest,
    pub environment: Digest,
    pub artifacts: Vec<AttestedRoleArtifact>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DevMaterializationAttestation {
    pub statement: DevMaterializationStatement,
    pub attestation: Digest,
}

impl DevMaterializationStatement {
    pub fn validate_against_lock(&self, locked: &LockedModule) -> Result<(), Error> {
        if self.format != MATERIALIZATION_ATTESTATION_FORMAT {
            return Err(Error::InvalidReleaseMetadata(format!(
                "unsupported materialization attestation format {}",
                self.format
            )));
        }
        let selection = locked.selection.as_ref().ok_or_else(|| {
            Error::InvalidReleaseMetadata(format!(
                "{}@{} has no locked capability selection",
                locked.path, locked.version
            ))
        })?;
        if selection.mode != LockedArtifactMode::SourceRecipe {
            return Err(Error::InvalidReleaseMetadata(format!(
                "{}@{} is not locked to a source recipe",
                locked.path, locked.version
            )));
        }
        let source_recipe = selection.source_recipe.as_ref().ok_or_else(|| {
            Error::InvalidReleaseMetadata("locked source recipe identity is missing".to_string())
        })?;
        let exact = self.module == locked.path
            && self.version == locked.version
            && self.capabilities == selection.capabilities
            && self.target == selection.target
            && self.toolchain == selection.toolchain
            && self.source_recipe == *source_recipe
            && self.schema == selection.schema
            && self.abi == selection.abi
            && self.vo_graph == selection.vo_graph
            && self.rust_graph == selection.rust_graph
            && self.js_graph == selection.js_graph
            && self.recipe_graph == selection.recipe_graph;
        if !exact {
            return Err(Error::InvalidReleaseMetadata(format!(
                "materialization statement does not exactly match the locked source recipe for {}@{}",
                locked.path, locked.version
            )));
        }
        if self.artifacts.is_empty() || self.artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::InvalidReleaseMetadata(format!(
                "materialization statement must contain 1..={} role artifacts",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }
        let expected_outputs = selection
            .source_outputs
            .iter()
            .map(|output| (&output.role, output.kind.as_str(), output.name.as_str()))
            .collect::<std::collections::BTreeSet<_>>();
        let actual_outputs = self
            .artifacts
            .iter()
            .map(|output| (&output.role, output.kind.as_str(), output.name.as_str()))
            .collect::<std::collections::BTreeSet<_>>();
        if actual_outputs != expected_outputs {
            return Err(Error::InvalidReleaseMetadata(
                "materialization artifacts differ from the locked source role outputs".to_string(),
            ));
        }
        let mut identities = std::collections::BTreeSet::new();
        for artifact in &self.artifacts {
            crate::schema::validate_file_name(&artifact.kind)
                .map_err(Error::InvalidReleaseMetadata)?;
            crate::schema::validate_file_name(&artifact.name)
                .map_err(Error::InvalidReleaseMetadata)?;
            if !identities.insert((
                artifact.role.clone(),
                artifact.kind.as_str(),
                artifact.name.as_str(),
            )) {
                return Err(Error::InvalidReleaseMetadata(
                    "materialization statement contains a duplicate role artifact".to_string(),
                ));
            }
            if artifact.size == 0 || artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
                return Err(Error::InvalidReleaseMetadata(format!(
                    "materialized role artifact {} size {} must be within 1..={}",
                    artifact.name,
                    artifact.size,
                    crate::MAX_MODULE_ARTIFACT_BYTES,
                )));
            }
        }
        Ok(())
    }

    fn canonical_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut statement = self.clone();
        statement.artifacts.sort_by(|left, right| {
            (&left.role, &left.kind, &left.name).cmp(&(&right.role, &right.kind, &right.name))
        });
        serde_json::to_vec(&statement).map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "failed to encode materialization statement: {error}"
            ))
        })
    }

    pub fn seal(self, locked: &LockedModule) -> Result<DevMaterializationAttestation, Error> {
        self.validate_against_lock(locked)?;
        let mut domain = b"vo-dev-materialization-attestation-v1\0".to_vec();
        domain.extend_from_slice(&self.canonical_bytes()?);
        Ok(DevMaterializationAttestation {
            statement: self,
            attestation: Digest::from_sha256(&domain),
        })
    }
}

impl DevMaterializationAttestation {
    pub fn validate_against_lock(&self, locked: &LockedModule) -> Result<(), Error> {
        self.statement.validate_against_lock(locked)?;
        let mut domain = b"vo-dev-materialization-attestation-v1\0".to_vec();
        domain.extend_from_slice(&self.statement.canonical_bytes()?);
        let expected = Digest::from_sha256(&domain);
        if self.attestation != expected {
            return Err(Error::InvalidReleaseMetadata(
                "materialization attestation digest mismatch".to_string(),
            ));
        }
        Ok(())
    }

    pub fn render(&self, locked: &LockedModule) -> Result<String, Error> {
        self.validate_against_lock(locked)?;
        let mut output = serde_json::to_string_pretty(self).map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "failed to encode materialization attestation: {error}"
            ))
        })?;
        output.push('\n');
        Ok(output)
    }
}

pub fn prepare_dev_materialization(
    locked: &LockedModule,
    inputs: Digest,
    environment: Digest,
    outputs: Vec<MaterializationOutput>,
) -> Result<PreparedDevMaterialization, Error> {
    let selection = locked.selection.as_ref().ok_or_else(|| {
        Error::InvalidReleaseMetadata("locked module has no capability selection".to_string())
    })?;
    if selection.mode != LockedArtifactMode::SourceRecipe {
        return Err(Error::InvalidReleaseMetadata(format!(
            "{}@{} is not locked to a source recipe",
            locked.path, locked.version,
        )));
    }
    let source_recipe = selection.source_recipe.clone().ok_or_else(|| {
        Error::InvalidReleaseMetadata("locked source recipe identity is missing".to_string())
    })?;
    if outputs.is_empty() || outputs.len() > crate::MAX_MODULE_ARTIFACTS {
        return Err(Error::InvalidReleaseMetadata(format!(
            "materialization must produce 1..={} outputs",
            crate::MAX_MODULE_ARTIFACTS,
        )));
    }

    let mut prepared = Vec::new();
    prepared
        .try_reserve(outputs.len())
        .map_err(|_| Error::SourceScan("failed to reserve materialization outputs".to_string()))?;
    for output in outputs {
        let size = u64::try_from(output.bytes.len()).unwrap_or(u64::MAX);
        let artifact = AttestedRoleArtifact {
            role: output.role,
            kind: output.kind,
            name: output.name,
            size,
            content_digest: Digest::from_sha256(&output.bytes),
            detached_manifest_digest: Digest::from_sha256(&output.detached_manifest),
        };
        DetachedArtifactManifest::parse(&output.detached_manifest)?
            .validate_against(locked, &artifact)?;
        let cache_relative_path = materialized_source_artifact_relative_path(locked, &artifact)?;
        let detached_manifest_relative_path =
            materialized_source_manifest_relative_path(locked, &artifact)?;
        prepared.push(PreparedMaterializedSourceArtifact {
            artifact,
            cache_relative_path,
            detached_manifest_relative_path,
            bytes: output.bytes,
            detached_manifest: output.detached_manifest,
        });
    }
    prepared.sort_by(|left, right| {
        (
            &left.artifact.role,
            &left.artifact.kind,
            &left.artifact.name,
        )
            .cmp(&(
                &right.artifact.role,
                &right.artifact.kind,
                &right.artifact.name,
            ))
    });
    let statement = DevMaterializationStatement {
        format: MATERIALIZATION_ATTESTATION_FORMAT,
        module: locked.path.clone(),
        version: locked.version.clone(),
        capabilities: selection.capabilities.clone(),
        target: selection.target.clone(),
        toolchain: selection.toolchain.clone(),
        source_recipe,
        schema: selection.schema.clone(),
        abi: selection.abi.clone(),
        vo_graph: selection.vo_graph.clone(),
        rust_graph: selection.rust_graph.clone(),
        js_graph: selection.js_graph.clone(),
        recipe_graph: selection.recipe_graph.clone(),
        inputs,
        environment,
        artifacts: prepared
            .iter()
            .map(|output| output.artifact.clone())
            .collect(),
    };
    let attestation = statement.seal(locked)?;
    Ok(PreparedDevMaterialization {
        attestation_cache_path: materialization_attestation_cache_path(locked)?,
        attestation,
        artifacts: prepared,
    })
}

/// Publish immutable source-build outputs first and the attestation commit
/// marker last. A crash can leave unreferenced content-addressed files, while
/// readiness can never observe a committed attestation with missing outputs.
pub fn publish_prepared_dev_materialization(
    cache_root: &std::path::Path,
    locked: &LockedModule,
    prepared: &PreparedDevMaterialization,
) -> Result<(), Error> {
    prepared.attestation.validate_against_lock(locked)?;
    let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(cache_root)?;
    let _identity_lock = mutation_lock.identity_lock(&format!(
        "source-materialization:{}@{}:{}",
        locked.path, locked.version, prepared.attestation.attestation,
    ))?;
    let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    mutation_lock.ensure_directory(&module_dir)?;

    for output in &prepared.artifacts {
        let found_size = u64::try_from(output.bytes.len()).unwrap_or(u64::MAX);
        let found_digest = Digest::from_sha256(&output.bytes);
        let found_manifest_digest = Digest::from_sha256(&output.detached_manifest);
        if found_size != output.artifact.size || found_digest != output.artifact.content_digest {
            return Err(Error::InvalidReleaseMetadata(format!(
                "prepared output {} bytes do not match its attested size and digest",
                output.artifact.name,
            )));
        }
        if output.detached_manifest.is_empty()
            || output.detached_manifest.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES
            || found_manifest_digest != output.artifact.detached_manifest_digest
        {
            return Err(Error::InvalidReleaseMetadata(format!(
                "prepared output {} detached manifest does not match its attested digest",
                output.artifact.name,
            )));
        }
        let expected_path = materialized_source_artifact_relative_path(locked, &output.artifact)?;
        if expected_path != output.cache_relative_path {
            return Err(Error::InvalidReleaseMetadata(format!(
                "prepared output {} has a non-canonical cache path",
                output.artifact.name,
            )));
        }
        let destination = module_dir.join(&output.cache_relative_path);
        publish_immutable_cache_file(
            &mutation_lock,
            &destination,
            &output.bytes,
            &format!(
                "source-output:{}@{}:{}:{}",
                locked.path,
                locked.version,
                output.artifact.role.as_str(),
                output.artifact.name,
            ),
        )?;
        let expected_manifest_path =
            materialized_source_manifest_relative_path(locked, &output.artifact)?;
        if expected_manifest_path != output.detached_manifest_relative_path {
            return Err(Error::InvalidReleaseMetadata(format!(
                "prepared output {} has a non-canonical detached manifest path",
                output.artifact.name,
            )));
        }
        let manifest_destination = module_dir.join(&output.detached_manifest_relative_path);
        publish_immutable_cache_file(
            &mutation_lock,
            &manifest_destination,
            &output.detached_manifest,
            &format!(
                "source-manifest:{}@{}:{}:{}",
                locked.path,
                locked.version,
                output.artifact.role.as_str(),
                output.artifact.name,
            ),
        )?;
    }

    let attestation_path = materialization_attestation_cache_path(locked)?;
    if attestation_path != prepared.attestation_cache_path {
        return Err(Error::InvalidReleaseMetadata(
            "prepared attestation has a non-canonical cache path".to_string(),
        ));
    }
    let attestation_bytes = prepared.attestation.render(locked)?.into_bytes();
    publish_immutable_cache_file(
        &mutation_lock,
        &attestation_path,
        &attestation_bytes,
        &format!(
            "source-attestation:{}@{}:{}",
            locked.path, locked.version, prepared.attestation.attestation,
        ),
    )?;
    load_materialized_source_artifacts(&mutation_lock.file_system(), locked)?;
    Ok(())
}

fn publish_immutable_cache_file(
    mutation_lock: &crate::cache::mutation_lock::CacheMutationLock,
    destination: &std::path::Path,
    bytes: &[u8],
    identity: &str,
) -> Result<(), Error> {
    match mutation_lock.entry_kind(destination)? {
        vo_common::vfs::FileSystemEntryKind::RegularFile => {
            let existing = mutation_lock
                .file_system()
                .read_bytes_limited(destination, bytes.len())
                .map_err(Error::Io)?;
            if existing == bytes {
                return Ok(());
            }
            return Err(Error::SourceScan(format!(
                "immutable materialization cache entry {} has different bytes",
                destination.display(),
            )));
        }
        vo_common::vfs::FileSystemEntryKind::Missing => {}
        kind => {
            return Err(Error::SourceScan(format!(
                "materialization cache destination {} contains invalid {kind:?} data",
                destination.display(),
            )));
        }
    }
    let parent = destination.parent().ok_or_else(|| {
        Error::SourceScan(format!(
            "materialization cache path has no parent: {}",
            destination.display(),
        ))
    })?;
    mutation_lock.ensure_directory(parent)?;
    let mut transaction = mutation_lock.begin_transaction(identity)?;
    transaction.write_file(std::path::Path::new("payload"), bytes)?;
    let staged = transaction.read_file(std::path::Path::new("payload"), bytes.len())?;
    if staged != bytes {
        return Err(Error::DigestMismatch {
            context: format!(
                "staged materialization cache entry {}",
                destination.display()
            ),
            expected: Digest::from_sha256(bytes).to_string(),
            found: Digest::from_sha256(&staged).to_string(),
        });
    }
    if let Err(error) = transaction.publish_file(std::path::Path::new("payload"), destination) {
        if matches!(&error, Error::Io(io) if io.kind() == std::io::ErrorKind::AlreadyExists) {
            let existing = mutation_lock
                .file_system()
                .read_bytes_limited(destination, bytes.len())
                .map_err(Error::Io)?;
            if existing == bytes {
                return Ok(());
            }
        }
        return Err(error);
    }
    Ok(())
}

pub fn materialization_attestation_cache_path(locked: &LockedModule) -> Result<PathBuf, Error> {
    let selection = locked.selection.as_ref().ok_or_else(|| {
        Error::InvalidReleaseMetadata("locked module has no capability selection".to_string())
    })?;
    let recipe = selection.source_recipe.as_ref().ok_or_else(|| {
        Error::InvalidReleaseMetadata("locked module has no source recipe".to_string())
    })?;
    let identity = format!(
        "vo-materialization-attestation-cache-v1\0{}\0{}\0{}\0{}\0{}\0{}\0{}",
        locked.path,
        locked.version,
        selection.capabilities.join("+"),
        selection.target,
        selection.toolchain,
        selection.recipe_graph,
        recipe,
    );
    let digest = Digest::from_sha256(identity.as_bytes());
    let hex = digest
        .as_str()
        .strip_prefix("sha256:")
        .expect("SHA-256 digest has a canonical prefix");
    Ok(PathBuf::from("materialization-attestations").join(format!("{hex}.json")))
}

pub fn materialized_source_artifact_relative_path(
    locked: &LockedModule,
    artifact: &AttestedRoleArtifact,
) -> Result<PathBuf, Error> {
    let selection = locked.selection.as_ref().ok_or_else(|| {
        Error::InvalidReleaseMetadata("locked module has no capability selection".to_string())
    })?;
    let recipe = selection.source_recipe.as_ref().ok_or_else(|| {
        Error::InvalidReleaseMetadata("locked module has no source recipe".to_string())
    })?;
    crate::schema::validate_file_name(&artifact.kind).map_err(Error::InvalidReleaseMetadata)?;
    crate::schema::validate_file_name(&artifact.name).map_err(Error::InvalidReleaseMetadata)?;
    let identity = format!(
        "vo-materialized-source-artifact-v1\0{}\0{}\0{}\0{}\0{}\0{}\0{}\0{}",
        locked.path,
        locked.version,
        selection.capabilities.join("+"),
        selection.target,
        selection.toolchain,
        recipe,
        artifact.role.as_str(),
        artifact.content_digest,
    );
    let digest = Digest::from_sha256(identity.as_bytes());
    let hex = digest
        .as_str()
        .strip_prefix("sha256:")
        .expect("SHA-256 digest has a canonical prefix");
    Ok(PathBuf::from("artifacts")
        .join("source")
        .join(hex)
        .join(artifact.role.as_str())
        .join(&artifact.kind)
        .join(&artifact.name))
}

pub fn materialized_source_manifest_relative_path(
    locked: &LockedModule,
    artifact: &AttestedRoleArtifact,
) -> Result<PathBuf, Error> {
    let artifact_path = materialized_source_artifact_relative_path(locked, artifact)?;
    let file_name = artifact_path
        .file_name()
        .and_then(std::ffi::OsStr::to_str)
        .ok_or_else(|| {
            Error::InvalidReleaseMetadata(
                "materialized source artifact path has no portable file name".to_string(),
            )
        })?;
    Ok(artifact_path.with_file_name(format!("{file_name}.manifest.json")))
}

/// Load and authenticate one development materialization from the read-only
/// cache. The attestation path is derived solely from the lock; every output
/// path additionally binds its actual content digest.
pub fn load_materialized_source_artifacts<F: vo_common::vfs::FileSystem>(
    fs: &F,
    locked: &LockedModule,
) -> Result<
    (
        DevMaterializationAttestation,
        Vec<MaterializedSourceArtifact>,
    ),
    Error,
> {
    let attestation_path = materialization_attestation_cache_path(locked)?;
    match fs.entry_kind(&attestation_path).map_err(Error::Io)? {
        vo_common::vfs::FileSystemEntryKind::RegularFile => {}
        kind => {
            return Err(Error::SourceScan(format!(
                "materialization attestation {} has invalid {kind:?} data",
                attestation_path.display(),
            )));
        }
    }
    let raw = fs
        .read_bytes_limited(&attestation_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(Error::Io)?;
    let attestation: DevMaterializationAttestation =
        serde_json::from_slice(&raw).map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "failed to parse materialization attestation {}: {error}",
                attestation_path.display(),
            ))
        })?;
    attestation.validate_against_lock(locked)?;

    let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
    let selection = locked
        .selection
        .as_ref()
        .expect("validated attestation lock");
    let mut outputs = Vec::new();
    outputs
        .try_reserve(attestation.statement.artifacts.len())
        .map_err(|_| {
            Error::SourceScan("failed to reserve materialized source artifacts".to_string())
        })?;
    for artifact in &attestation.statement.artifacts {
        let cache_relative_path = materialized_source_artifact_relative_path(locked, artifact)?;
        let detached_manifest_relative_path =
            materialized_source_manifest_relative_path(locked, artifact)?;
        let artifact_path = module_dir.join(&cache_relative_path);
        let detached_manifest_path = module_dir.join(&detached_manifest_relative_path);
        match fs.entry_kind(&artifact_path).map_err(Error::Io)? {
            vo_common::vfs::FileSystemEntryKind::RegularFile => {}
            kind => {
                return Err(Error::SourceScan(format!(
                    "materialized source artifact {} has invalid {kind:?} data",
                    artifact_path.display(),
                )));
            }
        }
        let bytes = fs
            .read_bytes_limited(&artifact_path, crate::MAX_MODULE_ARTIFACT_BYTES_USIZE)
            .map_err(Error::Io)?;
        let found_size = u64::try_from(bytes.len()).unwrap_or(u64::MAX);
        let found_digest = Digest::from_sha256(&bytes);
        if found_size != artifact.size || found_digest != artifact.content_digest {
            return Err(Error::DigestMismatch {
                context: format!(
                    "materialized source artifact {} for {} {}",
                    artifact.name, locked.path, locked.version
                ),
                expected: format!("{} ({} bytes)", artifact.content_digest, artifact.size),
                found: format!("{found_digest} ({found_size} bytes)"),
            });
        }
        match fs.entry_kind(&detached_manifest_path).map_err(Error::Io)? {
            vo_common::vfs::FileSystemEntryKind::RegularFile => {}
            kind => {
                return Err(Error::SourceScan(format!(
                    "materialized source detached manifest {} has invalid {kind:?} data",
                    detached_manifest_path.display(),
                )));
            }
        }
        let detached_manifest = fs
            .read_bytes_limited(&detached_manifest_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(Error::Io)?;
        let found_manifest_digest = Digest::from_sha256(&detached_manifest);
        if detached_manifest.is_empty()
            || found_manifest_digest != artifact.detached_manifest_digest
        {
            return Err(Error::DigestMismatch {
                context: format!(
                    "materialized source detached manifest {} for {} {}",
                    artifact.name, locked.path, locked.version,
                ),
                expected: artifact.detached_manifest_digest.to_string(),
                found: found_manifest_digest.to_string(),
            });
        }
        DetachedArtifactManifest::parse(&detached_manifest)?.validate_against(locked, artifact)?;
        let id = crate::identity::ArtifactId {
            kind: artifact.kind.clone(),
            target: selection.target.clone(),
            name: artifact.name.clone(),
        };
        id.validate().map_err(Error::InvalidReleaseMetadata)?;
        outputs.push(MaterializedSourceArtifact {
            role: artifact.role.clone(),
            id,
            size: artifact.size,
            digest: artifact.content_digest.clone(),
            detached_manifest_digest: artifact.detached_manifest_digest.clone(),
            cache_relative_path,
            detached_manifest_relative_path,
        });
    }
    Ok((attestation, outputs))
}
