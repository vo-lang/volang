use crate::digest::Digest;
use crate::identity::{ArtifactId, ModulePath};
use crate::schema::manifest::ReleaseManifest;
use crate::version::ExactVersion;
use crate::Error;
use std::collections::BTreeMap;
use std::sync::{Mutex, MutexGuard};

/// Repository identity derived from a module path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RepositoryId {
    pub owner: String,
    pub repo: String,
}

/// Trait for registry access — abstracted for testability.
/// The registry provides version discovery and manifest retrieval.
pub trait Registry: Send + Sync {
    /// List published release candidates for a module path.
    ///
    /// A candidate comes from registry indexing metadata and is not trusted to
    /// contain a complete or valid Volang release. Selection code must fetch
    /// and validate its manifest before choosing it.
    fn list_version_candidates(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error>;

    fn probe_module_path(&self, module: &ModulePath) -> Result<bool, Error> {
        probe_module_path_default(self, module)
    }

    /// Fetch the exact release-manifest bytes stored by the registry. The
    /// digest of these bytes is recorded as `release` in `vo.lock`.
    /// Typed values are always derived centrally from this snapshot.
    ///
    /// A transport-backed implementation must authenticate all provenance
    /// metadata owned by that transport before returning these bytes. For the
    /// GitHub registry this includes the release tag's commit and the complete
    /// release-asset inventory.
    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Vec<u8>, Error>;

    /// Parse already-fetched resolution bytes after the solver has charged
    /// them to its command-scoped budget. Registry descriptors use the public
    /// release schema; workspace overlays may recognize their own exact
    /// unpublished descriptor bytes.
    fn parse_resolution_manifest(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        raw: &[u8],
    ) -> Result<ReleaseManifest, Error> {
        parse_manifest_bytes(raw, module, version)
    }

    /// Fetch the source package bytes for a specific module version. The asset
    /// name must come from the authenticated release manifest.
    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error>;

    /// Fetch a target-specific artifact by its complete protocol identity. The
    /// identity must be declared by the authenticated release manifest.
    fn fetch_artifact(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        artifact: &ArtifactId,
    ) -> Result<Vec<u8>, Error>;
}

pub(crate) fn probe_module_path_default<R: Registry + ?Sized>(
    registry: &R,
    module: &ModulePath,
) -> Result<bool, Error> {
    let versions = match registry.list_version_candidates(module) {
        Ok(versions) => normalize_version_candidates(module, versions)?,
        Err(Error::RegistryNotFound { .. }) => return Ok(false),
        Err(error) => return Err(error),
    };
    let mut processed_manifest_bytes = 0usize;
    for version in filter_compatible_versions(module, &versions) {
        let raw = match registry.fetch_manifest_raw(module, &version) {
            Ok(raw) => raw,
            Err(error) if is_definitive_invalid_release_error(&error) => {
                continue;
            }
            Err(error) => return Err(error),
        };
        processed_manifest_bytes = charge_processed_manifest_bytes(
            processed_manifest_bytes,
            raw.len(),
            crate::MAX_SOLVER_MANIFEST_BYTES,
        )?;
        match parse_manifest_bytes(&raw, module, &version) {
            Ok(_) => return Ok(true),
            Err(error) if is_definitive_invalid_release_error(&error) => {}
            Err(error) => return Err(error),
        }
    }
    Ok(false)
}

fn lock_unpoisoned<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

#[derive(Default)]
struct RegistryOperationState {
    version_lists: BTreeMap<ModulePath, Result<Vec<ExactVersion>, Error>>,
    manifests: BTreeMap<(ModulePath, ExactVersion), Result<Vec<u8>, Error>>,
    probes: BTreeMap<ModulePath, Result<bool, Error>>,
    candidates: usize,
    manifest_bytes: usize,
    probe_count: usize,
    terminal_error: Option<Error>,
}

/// One command-scoped frozen registry view. It gives `mod add`, `mod tidy`,
/// solve, and materialization one aggregate work budget and one stable answer
/// for every metadata key, including failures.
pub(crate) struct RegistryOperation<'a> {
    inner: &'a dyn Registry,
    state: Mutex<RegistryOperationState>,
    metadata_gate: Mutex<()>,
    probe_gate: Mutex<()>,
}

impl<'a> RegistryOperation<'a> {
    pub(crate) fn new(inner: &'a dyn Registry) -> Self {
        Self {
            inner,
            state: Mutex::new(RegistryOperationState::default()),
            metadata_gate: Mutex::new(()),
            probe_gate: Mutex::new(()),
        }
    }
}

impl Registry for RegistryOperation<'_> {
    fn list_version_candidates(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        let state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        if let Some(cached) = state.version_lists.get(module) {
            return cached.clone();
        }
        drop(state);
        let _gate = lock_unpoisoned(&self.metadata_gate);
        let state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        if let Some(cached) = state.version_lists.get(module) {
            return cached.clone();
        }
        drop(state);
        let fetched = self.inner.list_version_candidates(module);
        let mut state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        let frozen = match fetched {
            Ok(candidates) => {
                let next = state
                    .candidates
                    .checked_add(candidates.len())
                    .ok_or_else(|| Error::ResolutionLimitExceeded {
                        resource: "registry candidate count for operation".to_string(),
                        limit: crate::MAX_SOLVER_CANDIDATES,
                    });
                match next {
                    Ok(next) if next <= crate::MAX_SOLVER_CANDIDATES => {
                        state.candidates = next;
                        Ok(candidates)
                    }
                    _ => {
                        let error = Error::ResolutionLimitExceeded {
                            resource: "registry candidate count for operation".to_string(),
                            limit: crate::MAX_SOLVER_CANDIDATES,
                        };
                        state.terminal_error = Some(error.clone());
                        Err(error)
                    }
                }
            }
            Err(error) => Err(error),
        };
        state.version_lists.insert(module.clone(), frozen.clone());
        frozen
    }

    fn probe_module_path(&self, module: &ModulePath) -> Result<bool, Error> {
        let state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        if let Some(cached) = state.probes.get(module) {
            return cached.clone();
        }
        drop(state);
        let _gate = lock_unpoisoned(&self.probe_gate);
        let state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        if let Some(cached) = state.probes.get(module) {
            return cached.clone();
        }
        drop(state);
        {
            let mut state = lock_unpoisoned(&self.state);
            let Some(next) = state.probe_count.checked_add(1) else {
                let error = Error::ResolutionLimitExceeded {
                    resource: "registry module-path probe count".to_string(),
                    limit: crate::MAX_SOLVER_CANDIDATES,
                };
                state.terminal_error = Some(error.clone());
                return Err(error);
            };
            if next > crate::MAX_SOLVER_CANDIDATES {
                let error = Error::ResolutionLimitExceeded {
                    resource: "registry module-path probe count".to_string(),
                    limit: crate::MAX_SOLVER_CANDIDATES,
                };
                state.terminal_error = Some(error.clone());
                return Err(error);
            }
            state.probe_count = next;
        }
        let result = probe_module_path_default(self, module);
        let mut state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        state.probes.insert(module.clone(), result.clone());
        result
    }

    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        let key = (module.clone(), version.clone());
        let state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        if let Some(cached) = state.manifests.get(&key) {
            return cached.clone();
        }
        drop(state);
        let _gate = lock_unpoisoned(&self.metadata_gate);
        let state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        if let Some(cached) = state.manifests.get(&key) {
            return cached.clone();
        }
        drop(state);
        let fetched = self.inner.fetch_manifest_raw(module, version);
        let mut state = lock_unpoisoned(&self.state);
        if let Some(error) = state.terminal_error.as_ref() {
            return Err(error.clone());
        }
        let frozen = match fetched {
            Ok(raw) => match charge_processed_manifest_bytes(
                state.manifest_bytes,
                raw.len(),
                crate::MAX_SOLVER_MANIFEST_BYTES,
            ) {
                Ok(next) => {
                    state.manifest_bytes = next;
                    Ok(raw)
                }
                Err(error) => {
                    state.terminal_error = Some(error.clone());
                    Err(error)
                }
            },
            Err(error) => Err(error),
        };
        state.manifests.insert(key, frozen.clone());
        frozen
    }

    fn parse_resolution_manifest(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        raw: &[u8],
    ) -> Result<ReleaseManifest, Error> {
        self.inner.parse_resolution_manifest(module, version, raw)
    }

    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        if let Some(error) = lock_unpoisoned(&self.state).terminal_error.as_ref() {
            return Err(error.clone());
        }
        let fetched = self.inner.fetch_source_package(module, version, asset_name);
        if let Some(error) = lock_unpoisoned(&self.state).terminal_error.as_ref() {
            return Err(error.clone());
        }
        fetched
    }

    fn fetch_artifact(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        artifact: &ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        if let Some(error) = lock_unpoisoned(&self.state).terminal_error.as_ref() {
            return Err(error.clone());
        }
        let fetched = self.inner.fetch_artifact(module, version, artifact);
        if let Some(error) = lock_unpoisoned(&self.state).terminal_error.as_ref() {
            return Err(error.clone());
        }
        fetched
    }
}

/// Whether a listed version is proven unusable from its own immutable bytes.
///
/// These failures may be skipped while evaluating other published versions.
/// Registry transport, filesystem, and solve-budget errors leave the candidate
/// set unknown and must propagate immediately.
pub fn is_definitive_invalid_release_error(error: &Error) -> bool {
    matches!(
        error,
        Error::InvalidReleaseMetadata(_) | Error::ManifestParse(_)
    )
}

/// Charge raw manifest bytes before parsing so malformed releases consume the
/// same solve-wide work budget as valid releases.
pub(crate) fn charge_processed_manifest_bytes(
    current: usize,
    additional: usize,
    limit: usize,
) -> Result<usize, Error> {
    let processed =
        current
            .checked_add(additional)
            .ok_or_else(|| Error::ResolutionLimitExceeded {
                resource: "processed manifest byte count".to_string(),
                limit,
            })?;
    if processed > limit {
        return Err(Error::ResolutionLimitExceeded {
            resource: "processed manifest byte count".to_string(),
            limit,
        });
    }
    Ok(processed)
}

/// Aggregate authority budget for release metadata recovered while a compact
/// lock graph is materialized or verified.
///
/// `vo.lock` deliberately omits release artifacts, so consumers must rebuild
/// the solver's graph-wide manifest-byte and artifact-count invariants from
/// the authenticated `vo.release.json` objects before acting on them.
#[derive(Debug, Default)]
pub(crate) struct MaterializedGraphBudget {
    manifest_bytes: usize,
    artifacts: usize,
}

impl MaterializedGraphBudget {
    pub(crate) fn charge_release(
        &mut self,
        manifest_bytes: usize,
        artifacts: usize,
    ) -> Result<(), Error> {
        let next_manifest_bytes = charge_processed_manifest_bytes(
            self.manifest_bytes,
            manifest_bytes,
            crate::MAX_SOLVER_MANIFEST_BYTES,
        )?;
        let next_artifacts = self
            .artifacts
            .checked_add(artifacts)
            .filter(|count| *count <= crate::MAX_SOLVER_GRAPH_ARTIFACTS)
            .ok_or_else(|| Error::ResolutionLimitExceeded {
                resource: "selected graph artifact count".to_string(),
                limit: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
            })?;
        self.manifest_bytes = next_manifest_bytes;
        self.artifacts = next_artifacts;
        Ok(())
    }
}

pub fn charge_registry_listing_bytes(current: usize, additional: usize) -> Result<usize, Error> {
    let processed =
        current
            .checked_add(additional)
            .ok_or_else(|| Error::ResolutionLimitExceeded {
                resource: "processed registry listing byte count".to_string(),
                limit: crate::MAX_REGISTRY_LISTING_BYTES,
            })?;
    if processed > crate::MAX_REGISTRY_LISTING_BYTES {
        return Err(Error::ResolutionLimitExceeded {
            resource: "processed registry listing byte count".to_string(),
            limit: crate::MAX_REGISTRY_LISTING_BYTES,
        });
    }
    Ok(processed)
}

/// Bound and canonicalize untrusted registry index results before selection.
pub fn normalize_version_candidates(
    module: &ModulePath,
    mut candidates: Vec<ExactVersion>,
) -> Result<Vec<ExactVersion>, Error> {
    if candidates.len() > crate::MAX_REGISTRY_RELEASES {
        return Err(Error::RegistryError(format!(
            "registry returned {} version candidates for {}, exceeding the {}-candidate limit",
            candidates.len(),
            module,
            crate::MAX_REGISTRY_RELEASES,
        )));
    }
    candidates.sort_by(|left, right| right.cmp(left));
    candidates.dedup();
    Ok(candidates)
}

/// Fetch a registry manifest with raw bytes as the sole typed authority.
///
/// Lock digests bind these bytes, so every consumer parses and identity-checks
/// this exact snapshot before using any field.
pub fn fetch_verified_manifest_raw<R: Registry + ?Sized>(
    registry: &R,
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<(ReleaseManifest, Vec<u8>), Error> {
    let raw = registry.fetch_manifest_raw(module, version)?;
    let manifest = parse_manifest_bytes(&raw, module, version)?;
    Ok((manifest, raw))
}

pub(crate) fn parse_manifest_bytes(
    raw: &[u8],
    module: &ModulePath,
    version: &ExactVersion,
) -> Result<ReleaseManifest, Error> {
    let content = std::str::from_utf8(raw).map_err(|error| {
        Error::ManifestParse(format!(
            "vo.release.json for {module} {version} is not valid UTF-8: {error}"
        ))
    })?;
    parse_requested_release_manifest(content, module, version)
}

/// Compute the GitHub repository identity from a canonical module path.
pub fn repository_id(module: &ModulePath) -> RepositoryId {
    RepositoryId {
        owner: module.owner().to_string(),
        repo: module.repo().to_string(),
    }
}

pub fn release_tag(module: &ModulePath, version: &ExactVersion) -> String {
    module.version_tag(version)
}

pub fn version_from_tag(module: &ModulePath, tag: &str) -> Option<ExactVersion> {
    let root = module.module_root();
    let version_str = if root == "." {
        tag
    } else {
        let prefix = format!("{}/", root);
        tag.strip_prefix(&prefix)?
    };
    ExactVersion::parse(version_str.strip_prefix('v')?).ok()
}

/// Build the GitHub release asset download URL for a module+version+asset.
///
/// `https://github.com/<owner>/<repo>/releases/download/<tag>/<asset_name>`
///
/// Single source of truth for release asset URLs, used by both the native
/// `GitHubRegistry` and the WASM fetch layer.
pub fn release_download_url(
    module: &ModulePath,
    version: &ExactVersion,
    asset_name: &str,
) -> String {
    let rid = repository_id(module);
    let tag = release_tag(module, version);
    format!(
        "https://github.com/{}/{}/releases/download/{}/{}",
        encode_url_path_component(&rid.owner),
        encode_url_path_component(&rid.repo),
        encode_url_path(&tag),
        encode_url_path_component(asset_name),
    )
}

/// Derive the complete canonical release-asset inventory from authenticated
/// `vo.release.json` bytes.
///
/// GitHub release assets are a flat namespace. Keeping this derivation in the
/// registry layer gives every transport one source of truth for the fixed
/// protocol assets, source archive, and extension artifacts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CanonicalReleaseAsset {
    pub(crate) size: u64,
    pub(crate) digest: Digest,
}

pub(crate) fn canonical_release_assets(
    manifest: &ReleaseManifest,
    release_manifest_raw: &[u8],
) -> Result<BTreeMap<String, CanonicalReleaseAsset>, Error> {
    let release_manifest_size = u64::try_from(release_manifest_raw.len()).map_err(|_| {
        Error::InvalidReleaseMetadata(
            "vo.release.json byte length cannot be represented by the release protocol".to_string(),
        )
    })?;
    let mut assets = BTreeMap::new();
    insert_canonical_release_asset(
        &mut assets,
        "vo.release.json".to_string(),
        release_manifest_size,
        Digest::from_sha256(release_manifest_raw),
    )?;
    insert_canonical_release_asset(
        &mut assets,
        manifest.source.name.clone(),
        manifest.source.size,
        manifest.source.digest.clone(),
    )?;
    for artifact in &manifest.artifacts {
        let asset_name =
            crate::artifact::artifact_release_asset_name(&artifact.id).map_err(|error| {
                Error::InvalidReleaseMetadata(format!(
                    "cannot derive release asset for artifact {}: {error}",
                    artifact.id
                ))
            })?;
        insert_canonical_release_asset(
            &mut assets,
            asset_name,
            artifact.size,
            artifact.digest.clone(),
        )?;
    }
    Ok(assets)
}

fn insert_canonical_release_asset(
    assets: &mut BTreeMap<String, CanonicalReleaseAsset>,
    name: String,
    size: u64,
    digest: Digest,
) -> Result<(), Error> {
    if assets
        .insert(name.clone(), CanonicalReleaseAsset { size, digest })
        .is_some()
    {
        return Err(Error::InvalidReleaseMetadata(format!(
            "canonical release asset name {name:?} is declared more than once"
        )));
    }
    Ok(())
}

pub(crate) fn encode_url_path(value: &str) -> String {
    value
        .split('/')
        .map(encode_url_path_component)
        .collect::<Vec<_>>()
        .join("/")
}

pub(crate) fn encode_url_path_component(value: &str) -> String {
    const HEX: &[u8; 16] = b"0123456789ABCDEF";
    let mut encoded = String::with_capacity(value.len());
    for byte in value.bytes() {
        if byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'.' | b'_' | b'~') {
            encoded.push(char::from(byte));
        } else {
            encoded.push('%');
            encoded.push(char::from(HEX[usize::from(byte >> 4)]));
            encoded.push(char::from(HEX[usize::from(byte & 0x0f)]));
        }
    }
    encoded
}

pub fn parse_requested_release_manifest(
    content: &str,
    expected_module: &ModulePath,
    expected_version: &ExactVersion,
) -> Result<ReleaseManifest, Error> {
    let manifest = ReleaseManifest::parse(content).map_err(|error| {
        Error::InvalidReleaseMetadata(format!(
            "invalid vo.release.json for {}@{}: {}",
            expected_module, expected_version, error,
        ))
    })?;
    validate_manifest(&manifest, expected_module, expected_version)?;
    Ok(manifest)
}

pub fn parse_requested_release_manifest_for_spec(
    module: &str,
    version: &str,
    content: &str,
) -> Result<ReleaseManifest, Error> {
    let expected_module = ModulePath::parse(module)?;
    let expected_version = ExactVersion::parse(version)?;
    parse_requested_release_manifest(content, &expected_module, &expected_version)
}

/// Validate that a release manifest is consistent with the module path and version
/// it was fetched for.
pub fn validate_manifest(
    manifest: &ReleaseManifest,
    expected_module: &ModulePath,
    expected_version: &ExactVersion,
) -> Result<(), Error> {
    manifest.render().map_err(|error| {
        Error::InvalidReleaseMetadata(format!("manifest failed canonical validation: {error}"))
    })?;
    if manifest.module != *expected_module {
        return Err(Error::InvalidReleaseMetadata(format!(
            "manifest module '{}' does not match expected '{}'",
            manifest.module, expected_module
        )));
    }
    if manifest.version != *expected_version {
        return Err(Error::InvalidReleaseMetadata(format!(
            "manifest version '{}' does not match expected '{}'",
            manifest.version, expected_version
        )));
    }
    // Verify major version compatibility
    if !expected_module.accepts_version(expected_version) {
        return Err(Error::InvalidReleaseMetadata(format!(
            "version {} is not compatible with module path {}",
            expected_version, expected_module
        )));
    }
    Ok(())
}

/// Filter versions to only those compatible with the module path's major version rule.
pub fn filter_compatible_versions(
    module: &ModulePath,
    versions: &[ExactVersion],
) -> Vec<ExactVersion> {
    versions
        .iter()
        .filter(|v| module.accepts_version(v))
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[derive(Default)]
    struct CountingRegistry {
        list_calls: AtomicUsize,
        manifest_calls: AtomicUsize,
        source_calls: AtomicUsize,
    }

    impl Registry for CountingRegistry {
        fn list_version_candidates(
            &self,
            _module: &ModulePath,
        ) -> Result<Vec<ExactVersion>, Error> {
            self.list_calls.fetch_add(1, Ordering::SeqCst);
            Ok(vec![ExactVersion::parse("1.0.0").unwrap()])
        }

        fn fetch_manifest_raw(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
        ) -> Result<Vec<u8>, Error> {
            let call = self.manifest_calls.fetch_add(1, Ordering::SeqCst);
            Ok(format!("manifest snapshot {call}").into_bytes())
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            self.source_calls.fetch_add(1, Ordering::SeqCst);
            Ok(Vec::new())
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _artifact: &ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            Ok(Vec::new())
        }
    }

    #[test]
    fn test_repository_id() {
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let rid = repository_id(&mp);
        assert_eq!(rid.owner, "acme");
        assert_eq!(rid.repo, "lib");
    }

    #[test]
    fn test_repository_id_subdir() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        let rid = repository_id(&mp);
        assert_eq!(rid.owner, "acme");
        assert_eq!(rid.repo, "mono");
    }

    #[test]
    fn test_version_tag_root() {
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let v = ExactVersion::parse("1.4.2").unwrap();
        assert_eq!(mp.version_tag(&v), "v1.4.2");
    }

    #[test]
    fn test_version_tag_nested() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        let v = ExactVersion::parse("0.8.0").unwrap();
        assert_eq!(mp.version_tag(&v), "graphics/v0.8.0");
    }

    #[test]
    fn release_download_url_encodes_asset_components_and_preserves_tag_segments() {
        let module = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        let version = ExactVersion::parse("0.8.0").unwrap();
        assert_eq!(
            release_download_url(&module, &version, "a#% 中.tar.gz"),
            "https://github.com/acme/mono/releases/download/graphics/v0.8.0/a%23%25%20%E4%B8%AD.tar.gz"
        );
    }

    #[test]
    fn test_version_from_tag_nested() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        let v = version_from_tag(&mp, "graphics/v2/v2.1.0").unwrap();
        assert_eq!(v.to_string(), "2.1.0");
    }

    #[test]
    fn test_parse_requested_release_manifest() {
        let mut manifest = parse_requested_release_manifest(
            r#"{
  "format": 1,
  "module": "github.com/acme/lib",
  "version": "1.2.3",
  "vo": "0.1.0",
  "intent": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
  "dependencies": [],
  "source": {
    "name": "source.tar.gz",
    "size": 3,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "tree": "sha256:ca3d163bab055381827226140568f3bef7eaac187cebd76878e0b63e9e442356"
  },
  "artifacts": []
}
"#,
            &ModulePath::parse("github.com/acme/lib").unwrap(),
            &ExactVersion::parse("1.2.3").unwrap(),
        )
        .unwrap();
        assert_eq!(manifest.module.as_str(), "github.com/acme/lib");

        manifest.source.name = "../forged.tar.gz".to_string();
        let error = validate_manifest(
            &manifest,
            &ModulePath::parse("github.com/acme/lib").unwrap(),
            &ExactVersion::parse("1.2.3").unwrap(),
        )
        .unwrap_err();
        assert!(
            error.to_string().contains("canonical validation"),
            "{error}"
        );
    }

    #[test]
    fn test_filter_compatible() {
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let versions = vec![
            ExactVersion::parse("0.1.0").unwrap(),
            ExactVersion::parse("1.0.0").unwrap(),
            ExactVersion::parse("2.0.0").unwrap(),
        ];
        let compat = filter_compatible_versions(&mp, &versions);
        assert_eq!(compat.len(), 2);
        assert_eq!(compat[0].to_string(), "0.1.0");
        assert_eq!(compat[1].to_string(), "1.0.0");
    }

    #[test]
    fn version_candidates_are_bounded_unique_and_stably_descending() {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let v1 = ExactVersion::parse("1.0.0").unwrap();
        let v0 = ExactVersion::parse("0.9.0").unwrap();
        assert_eq!(
            normalize_version_candidates(&module, vec![v0.clone(), v1.clone()]).unwrap(),
            vec![v1.clone(), v0],
        );

        assert_eq!(
            normalize_version_candidates(&module, vec![v1.clone(), v1.clone()]).unwrap(),
            vec![v1.clone()],
        );

        let oversized = normalize_version_candidates(
            &module,
            vec![v1; crate::MAX_REGISTRY_RELEASES.saturating_add(1)],
        )
        .unwrap_err();
        assert!(oversized.to_string().contains("limit"), "{oversized}");
    }

    #[test]
    fn registry_operation_freezes_metadata_once_under_concurrency() {
        let inner = CountingRegistry::default();
        let operation = RegistryOperation::new(&inner);
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.0.0").unwrap();

        std::thread::scope(|scope| {
            let mut workers = Vec::new();
            for _ in 0..8 {
                workers.push(scope.spawn(|| {
                    let versions = operation.list_version_candidates(&module).unwrap();
                    let raw = operation.fetch_manifest_raw(&module, &version).unwrap();
                    (versions, raw)
                }));
            }
            let first = workers.remove(0).join().unwrap();
            for worker in workers {
                assert_eq!(worker.join().unwrap(), first);
            }
        });

        assert_eq!(inner.list_calls.load(Ordering::SeqCst), 1);
        assert_eq!(inner.manifest_calls.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn registry_operation_terminal_failure_dominates_an_inflight_success() {
        use std::sync::Barrier;

        struct BlockingRegistry {
            entered: Barrier,
            release: Barrier,
        }

        impl Registry for BlockingRegistry {
            fn list_version_candidates(
                &self,
                _module: &ModulePath,
            ) -> Result<Vec<ExactVersion>, Error> {
                self.entered.wait();
                self.release.wait();
                Ok(vec![ExactVersion::parse("1.0.0").unwrap()])
            }

            fn fetch_manifest_raw(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
            ) -> Result<Vec<u8>, Error> {
                unreachable!()
            }

            fn fetch_source_package(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
                _asset_name: &str,
            ) -> Result<Vec<u8>, Error> {
                unreachable!()
            }

            fn fetch_artifact(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
                _artifact: &ArtifactId,
            ) -> Result<Vec<u8>, Error> {
                unreachable!()
            }
        }

        let inner = BlockingRegistry {
            entered: Barrier::new(2),
            release: Barrier::new(2),
        };
        let operation = RegistryOperation::new(&inner);
        let module = ModulePath::parse("github.com/acme/lib").unwrap();

        std::thread::scope(|scope| {
            let worker = scope.spawn(|| operation.list_version_candidates(&module));
            inner.entered.wait();
            lock_unpoisoned(&operation.state).terminal_error =
                Some(Error::ResolutionLimitExceeded {
                    resource: "concurrent registry operation".to_string(),
                    limit: 1,
                });
            inner.release.wait();
            assert!(matches!(
                worker.join().unwrap(),
                Err(Error::ResolutionLimitExceeded { .. })
            ));
        });
    }

    #[test]
    fn registry_operation_budget_failure_is_terminal_for_payload_fetches() {
        let inner = CountingRegistry::default();
        let operation = RegistryOperation::new(&inner);
        lock_unpoisoned(&operation.state).candidates = crate::MAX_SOLVER_CANDIDATES;
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let version = ExactVersion::parse("1.0.0").unwrap();

        assert!(matches!(
            operation.list_version_candidates(&module),
            Err(Error::ResolutionLimitExceeded { .. })
        ));
        assert!(matches!(
            operation.fetch_source_package(&module, &version, "source.tar.gz"),
            Err(Error::ResolutionLimitExceeded { .. })
        ));
        assert_eq!(inner.source_calls.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn materialized_graph_budget_restores_compact_lock_limits() {
        let mut budget = MaterializedGraphBudget::default();
        budget
            .charge_release(1, crate::MAX_SOLVER_GRAPH_ARTIFACTS)
            .unwrap();
        let error = budget.charge_release(1, 1).unwrap_err();
        assert!(matches!(
            error,
            Error::ResolutionLimitExceeded {
                ref resource,
                limit: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
            } if resource == "selected graph artifact count"
        ));
        assert_eq!(budget.manifest_bytes, 1);
        assert_eq!(budget.artifacts, crate::MAX_SOLVER_GRAPH_ARTIFACTS);

        let mut budget = MaterializedGraphBudget::default();
        budget
            .charge_release(crate::MAX_SOLVER_MANIFEST_BYTES, 0)
            .unwrap();
        let error = budget.charge_release(1, 0).unwrap_err();
        assert!(matches!(
            error,
            Error::ResolutionLimitExceeded {
                ref resource,
                limit: crate::MAX_SOLVER_MANIFEST_BYTES,
            } if resource == "processed manifest byte count"
        ));
        assert_eq!(budget.manifest_bytes, crate::MAX_SOLVER_MANIFEST_BYTES);
        assert_eq!(budget.artifacts, 0);
    }

    #[test]
    fn probe_treats_only_definitively_invalid_releases_as_absent() {
        struct InvalidReleaseRegistry;
        impl Registry for InvalidReleaseRegistry {
            fn list_version_candidates(
                &self,
                _module: &ModulePath,
            ) -> Result<Vec<ExactVersion>, Error> {
                Ok(vec![ExactVersion::parse("1.0.0").unwrap()])
            }

            fn fetch_manifest_raw(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
            ) -> Result<Vec<u8>, Error> {
                Err(Error::InvalidReleaseMetadata("invalid release".to_string()))
            }

            fn fetch_source_package(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
                _asset_name: &str,
            ) -> Result<Vec<u8>, Error> {
                unreachable!()
            }

            fn fetch_artifact(
                &self,
                _module: &ModulePath,
                _version: &ExactVersion,
                _artifact: &ArtifactId,
            ) -> Result<Vec<u8>, Error> {
                unreachable!()
            }
        }

        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        assert!(!InvalidReleaseRegistry.probe_module_path(&module).unwrap());
    }
}
