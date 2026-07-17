use std::collections::BTreeSet;
use std::fmt;
use std::path::{Path, PathBuf};

use vo_common::vfs::FileSystem;

use crate::artifact::required_artifacts_for_target;
use crate::cache::layout;
use crate::cache::validate::{
    validate_installed_artifact, validate_installed_module_with_metadata, InstalledModuleError,
};
use crate::digest::Digest;
use crate::ext_manifest::ExtensionManifest;
use crate::identity::{ArtifactId, ModulePath};
use crate::schema::lockfile::{validate_locked_module_graph, LockedModule};
use crate::version::ExactVersion;
use crate::Error;

const WASM_TARGET: &str = "wasm32-unknown-unknown";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedArtifact {
    id: ArtifactId,
    size: u64,
    digest: Digest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadyModule {
    module: ModulePath,
    version: ExactVersion,
    target: String,
    artifacts: Vec<ResolvedArtifact>,
    ext_manifest: Option<ExtensionManifest>,
}

impl ResolvedArtifact {
    pub fn try_new(id: ArtifactId, size: u64, digest: Digest) -> Result<Self, String> {
        id.validate()?;
        if size == 0 || size > crate::MAX_MODULE_ARTIFACT_BYTES {
            return Err(format!(
                "artifact {} size {} must be within 1..={}",
                id,
                size,
                crate::MAX_MODULE_ARTIFACT_BYTES,
            ));
        }
        Ok(Self { id, size, digest })
    }

    pub fn id(&self) -> &ArtifactId {
        &self.id
    }

    pub fn size(&self) -> u64 {
        self.size
    }

    pub fn digest(&self) -> &Digest {
        &self.digest
    }

    /// Canonical path below a module cache directory.
    pub fn cache_relative_path(&self) -> PathBuf {
        crate::artifact::artifact_relative_path(&self.id)
            .expect("ResolvedArtifact identities are validated at construction")
    }
}

impl ReadyModule {
    pub fn try_new(
        module: ModulePath,
        version: ExactVersion,
        target: impl Into<String>,
        mut artifacts: Vec<ResolvedArtifact>,
        ext_manifest: Option<ExtensionManifest>,
    ) -> Result<Self, String> {
        let target = target.into();
        if !module.accepts_version(&version) {
            return Err(format!(
                "module {} does not accept version {}",
                module, version,
            ));
        }
        validate_ready_target(&target)?;
        if let Some(manifest) = ext_manifest.as_ref() {
            manifest.validate().map_err(|error| error.to_string())?;
            if requires_declared_native_target(manifest, &target) {
                return Err(format!(
                    "vo.mod does not declare extension-native support for target {target}"
                ));
            }
        }
        if artifacts.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(format!(
                "ready module contains {} artifacts, exceeding the {}-artifact limit",
                artifacts.len(),
                crate::MAX_MODULE_ARTIFACTS,
            ));
        }

        let mut kinds = BTreeSet::new();
        let mut actual_ids = BTreeSet::new();
        for artifact in &artifacts {
            artifact.id.validate()?;
            if artifact.id.target != target {
                return Err(format!(
                    "artifact {} targets {}, while the ready module targets {}",
                    artifact.id, artifact.id.target, target,
                ));
            }
            if !kinds.insert(artifact.id.kind.as_str()) {
                return Err(format!(
                    "ready module contains duplicate {} artifacts for target {}",
                    artifact.id.kind, target,
                ));
            }
            if !actual_ids.insert(artifact.id.clone()) {
                return Err(format!(
                    "ready module contains duplicate artifact {}",
                    artifact.id
                ));
            }
        }

        let expected_ids = ext_manifest
            .as_ref()
            .into_iter()
            .flat_map(ExtensionManifest::declared_artifact_ids)
            .filter(|artifact| artifact.target == target)
            .map(|artifact| ArtifactId {
                kind: artifact.kind,
                target: artifact.target,
                name: artifact.name,
            })
            .collect::<BTreeSet<_>>();
        if actual_ids != expected_ids {
            return Err(format!(
                "resolved artifacts do not match vo.mod declarations for target {}: expected {}, found {}",
                target,
                format_artifact_ids(&expected_ids),
                format_artifact_ids(&actual_ids),
            ));
        }

        artifacts.sort_by(|left, right| left.id.cmp(&right.id));
        Ok(Self {
            module,
            version,
            target,
            artifacts,
            ext_manifest,
        })
    }

    pub fn module(&self) -> &ModulePath {
        &self.module
    }

    pub fn version(&self) -> &ExactVersion {
        &self.version
    }

    pub fn target(&self) -> &str {
        &self.target
    }

    pub fn module_dir(&self) -> PathBuf {
        layout::relative_module_dir(&self.module, &self.version)
    }

    pub fn artifacts(&self) -> &[ResolvedArtifact] {
        &self.artifacts
    }

    pub fn ext_manifest(&self) -> Option<&ExtensionManifest> {
        self.ext_manifest.as_ref()
    }
}

fn validate_ready_target(target: &str) -> Result<(), String> {
    crate::schema::validate_portable_path_component(target)
        .map_err(|_| "ready-module target must be a portable path component".to_string())?;
    if target.matches('-').count() < 2
        || target.split('-').any(|segment| {
            segment.is_empty()
                || !segment.chars().all(|ch| {
                    ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '_' || ch == '.'
                })
        })
    {
        return Err("ready-module target must be a canonical Rust target triple".to_string());
    }
    Ok(())
}

fn format_artifact_ids(ids: &BTreeSet<ArtifactId>) -> String {
    if ids.is_empty() {
        return "none".to_string();
    }
    crate::summarize_diagnostic_items(ids.iter().map(ToString::to_string), "artifact(s)")
}

#[derive(Debug)]
pub enum ModuleReadiness {
    Ready(Box<ReadyModule>),
    NotReady(ReadinessFailure),
}

#[derive(Debug)]
pub enum ReadinessFailure {
    LockedGraphInvalid {
        error: Box<Error>,
    },
    SourceNotReady {
        error: Box<InstalledModuleError>,
    },
    UnsupportedNativeTarget {
        module: String,
        version: String,
        target: String,
        manifest_path: PathBuf,
    },
    ArtifactResolutionFailed {
        module: String,
        version: String,
        manifest_path: PathBuf,
        error: Box<Error>,
    },
    ArtifactNotReady {
        module: String,
        version: String,
        artifact_path: PathBuf,
        error: Box<InstalledModuleError>,
    },
}

pub fn check_module_readiness<F: FileSystem>(
    fs: &F,
    locked: &LockedModule,
    target: &str,
) -> ModuleReadiness {
    let mut budget = crate::registry::MaterializedGraphBudget::default();
    check_module_readiness_with_budget(fs, locked, target, &mut budget)
}

fn check_module_readiness_with_budget<F: FileSystem>(
    fs: &F,
    locked: &LockedModule,
    target: &str,
    budget: &mut crate::registry::MaterializedGraphBudget,
) -> ModuleReadiness {
    let module_dir = layout::relative_module_dir(&locked.path, &locked.version);
    let manifest_path = module_dir.join("vo.mod");
    if let Err(error) =
        crate::schema::lockfile::validate_materialized_module_limits(std::slice::from_ref(locked))
    {
        return ModuleReadiness::NotReady(ReadinessFailure::LockedGraphInvalid {
            error: Box::new(error),
        });
    }
    if let Err(detail) = validate_ready_target(target) {
        return ModuleReadiness::NotReady(ReadinessFailure::ArtifactResolutionFailed {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            manifest_path: scoped_path(fs, &manifest_path),
            error: Box::new(Error::InvalidReleaseMetadata(detail)),
        });
    }
    let metadata = match validate_installed_module_with_metadata(fs, &module_dir, locked) {
        Ok(metadata) => metadata,
        Err(error) => {
            return ModuleReadiness::NotReady(ReadinessFailure::SourceNotReady {
                error: Box::new(error),
            });
        }
    };
    if let Err(error) = budget.charge_release(
        metadata.release_manifest_bytes,
        metadata.release.artifacts.len(),
    ) {
        return ModuleReadiness::NotReady(ReadinessFailure::LockedGraphInvalid {
            error: Box::new(error),
        });
    }

    let ext_manifest = metadata.extension;
    let published_artifacts = metadata.release.artifacts;

    if ext_manifest
        .as_ref()
        .is_some_and(|manifest| requires_declared_native_target(manifest, target))
    {
        return ModuleReadiness::NotReady(ReadinessFailure::UnsupportedNativeTarget {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            target: target.to_string(),
            manifest_path: scoped_path(fs, &manifest_path),
        });
    }

    let required = match required_artifacts_for_target(
        locked,
        &published_artifacts,
        ext_manifest.as_ref(),
        target,
    ) {
        Ok(required) => required,
        Err(error) => {
            return ModuleReadiness::NotReady(ReadinessFailure::ArtifactResolutionFailed {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                manifest_path: scoped_path(fs, &manifest_path),
                error: Box::new(error),
            });
        }
    };

    let mut artifacts = Vec::with_capacity(required.len());
    for required_artifact in required {
        let artifact_path = module_dir.join(&required_artifact.cache_relative_path);
        if let Err(error) =
            validate_installed_artifact(fs, &module_dir, locked, &required_artifact.artifact.id)
        {
            return ModuleReadiness::NotReady(ReadinessFailure::ArtifactNotReady {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                artifact_path: scoped_path(fs, &artifact_path),
                error: Box::new(error),
            });
        }
        let resolved = match ResolvedArtifact::try_new(
            required_artifact.artifact.id.clone(),
            required_artifact.artifact.size,
            required_artifact.artifact.digest.clone(),
        ) {
            Ok(resolved) => resolved,
            Err(detail) => {
                return ModuleReadiness::NotReady(ReadinessFailure::ArtifactResolutionFailed {
                    module: locked.path.as_str().to_string(),
                    version: locked.version.to_string(),
                    manifest_path: scoped_path(fs, &manifest_path),
                    error: Box::new(Error::InvalidReleaseMetadata(detail)),
                });
            }
        };
        artifacts.push(resolved);
    }

    match ReadyModule::try_new(
        locked.path.clone(),
        locked.version.clone(),
        target,
        artifacts,
        ext_manifest,
    ) {
        Ok(ready) => ModuleReadiness::Ready(Box::new(ready)),
        Err(detail) => ModuleReadiness::NotReady(ReadinessFailure::ArtifactResolutionFailed {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            manifest_path: scoped_path(fs, &manifest_path),
            error: Box::new(Error::InvalidReleaseMetadata(detail)),
        }),
    }
}

pub fn check_project_readiness<F: FileSystem>(
    fs: &F,
    locked_modules: &[LockedModule],
    target: &str,
) -> Result<Vec<ReadyModule>, ReadinessFailure> {
    validate_locked_module_graph(locked_modules).map_err(|error| {
        ReadinessFailure::LockedGraphInvalid {
            error: Box::new(error),
        }
    })?;
    check_materialized_modules_readiness(fs, locked_modules, target)
}

/// Check source and artifact readiness for an already-authorized materialized
/// subset of a lock graph.
///
/// Workspace sources may remove registry modules from the materialized set,
/// so callers must first validate the complete root `vo.mod`/`vo.lock` graph.
pub fn check_materialized_modules_readiness<F: FileSystem>(
    fs: &F,
    locked_modules: &[LockedModule],
    target: &str,
) -> Result<Vec<ReadyModule>, ReadinessFailure> {
    crate::schema::lockfile::validate_materialized_module_limits(locked_modules).map_err(
        |error| ReadinessFailure::LockedGraphInvalid {
            error: Box::new(error),
        },
    )?;
    let mut budget = crate::registry::MaterializedGraphBudget::default();
    let mut ready = Vec::with_capacity(locked_modules.len());
    for locked in locked_modules {
        match check_module_readiness_with_budget(fs, locked, target, &mut budget) {
            ModuleReadiness::Ready(module) => ready.push(*module),
            ModuleReadiness::NotReady(failure) => return Err(failure),
        }
    }
    Ok(ready)
}

fn scoped_path<F: FileSystem>(fs: &F, path: &Path) -> PathBuf {
    match fs.root() {
        Some(root) => root.join(path),
        None => path.to_path_buf(),
    }
}

fn requires_declared_native_target(ext_manifest: &ExtensionManifest, target: &str) -> bool {
    target != WASM_TARGET && ext_manifest.native.is_some() && !ext_manifest.supports_target(target)
}

impl fmt::Display for ReadinessFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LockedGraphInvalid { error } => write!(f, "{}", error),
            Self::SourceNotReady { error } => write!(f, "{}", error),
            Self::UnsupportedNativeTarget {
                module,
                version,
                target,
                ..
            } => {
                write!(
                    f,
                    "vo.mod does not declare extension-native support for target {} in {}@{}",
                    target, module, version,
                )
            }
            Self::ArtifactResolutionFailed { error, .. } => write!(f, "{}", error),
            Self::ArtifactNotReady { error, .. } => write!(f, "{}", error),
        }
    }
}

impl std::error::Error for ReadinessFailure {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    use crate::cache::layout;
    use crate::digest::Digest;
    use crate::ext_manifest::parse_ext_manifest_content;
    use crate::identity::{ArtifactId, ModulePath};
    use crate::schema::lockfile::{LockedDependency, LockedModule};
    use crate::schema::manifest::{
        ManifestArtifact, ManifestPackage, ManifestSource, ReleaseManifest,
    };
    use crate::schema::{PackageManifest, SourceFileEntry};
    use crate::version::ToolchainConstraint;
    use crate::version::{DepConstraint, ExactVersion};
    use vo_common::vfs::MemoryFs;

    fn published_artifact(kind: &str, target: &str, name: &str, bytes: &[u8]) -> ManifestArtifact {
        ManifestArtifact {
            id: ArtifactId {
                kind: kind.to_string(),
                target: target.to_string(),
                name: name.to_string(),
            },
            size: bytes.len() as u64,
            digest: Digest::from_sha256(bytes),
        }
    }

    fn cached_mod_content(module: &ModulePath, ext_manifest_content: Option<&str>) -> String {
        let mut content = format!("module = \"{module}\"\nvo = \"^0.1.0\"\n");
        if let Some(extension) = ext_manifest_content {
            content.push('\n');
            content.push_str(extension);
        }
        content
    }

    fn locked_module(
        version: &str,
        artifacts: Vec<ManifestArtifact>,
        ext_manifest_content: Option<&str>,
    ) -> (LockedModule, String) {
        locked_module_for(
            "github.com/acme/demo",
            version,
            artifacts,
            ext_manifest_content,
        )
    }

    fn locked_module_for(
        module: &str,
        version: &str,
        mut artifacts: Vec<ManifestArtifact>,
        ext_manifest_content: Option<&str>,
    ) -> (LockedModule, String) {
        let module = ModulePath::parse(module).unwrap();
        let mod_content = cached_mod_content(&module, ext_manifest_content);
        let package_content = cached_package_content(&mod_content);
        artifacts.sort_by(|left, right| left.id.cmp(&right.id));
        let manifest = ReleaseManifest {
            schema_version: 2,
            module,
            version: ExactVersion::parse(version).unwrap(),
            commit: "1111111111111111111111111111111111111111".to_string(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            dependencies: Vec::new(),
            source: ManifestSource {
                name: crate::schema::manifest::SOURCE_ARCHIVE_ASSET_NAME.to_string(),
                size: 3,
                digest: Digest::parse(
                    "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                )
                .unwrap(),
            },
            package: ManifestPackage {
                size: package_content.len() as u64,
                digest: Digest::from_sha256(&package_content),
            },
            artifacts,
        };
        let release_manifest_content = manifest.render().unwrap();
        let locked = crate::lock::locked_module_from_manifest_raw(
            &manifest,
            release_manifest_content.as_bytes(),
        );
        (locked, release_manifest_content)
    }

    fn cached_package_content(mod_content: &str) -> Vec<u8> {
        PackageManifest {
            schema_version: 1,
            files: vec![SourceFileEntry {
                path: "vo.mod".to_string(),
                mode: crate::schema::SourceFileMode::Regular,
                size: mod_content.len() as u64,
                digest: Digest::from_sha256(mod_content.as_bytes()),
            }],
        }
        .render()
        .unwrap()
    }

    fn native_manifest(target: &str) -> String {
        format!(
            r#"
[extension]
name = "demo"

[extension.native]
targets = ["{target}"]
"#,
        )
    }

    fn wasm_manifest(wasm: &str, js: &str) -> String {
        format!(
            r#"
[extension]
name = "demo"

[extension.wasm]
kind = "bindgen"
wasm = "{wasm}"
js = "{js}"
"#,
        )
    }

    fn parse_manifest(content: &str) -> crate::ext_manifest::ExtensionManifest {
        let module = ModulePath::parse("github.com/acme/demo").unwrap();
        parse_ext_manifest_content(
            &cached_mod_content(&module, Some(content)),
            Path::new("vo.mod"),
        )
        .unwrap()
    }

    fn populate_cached_module(
        fs: &mut MemoryFs,
        locked: &LockedModule,
        release_manifest_content: &str,
        ext_manifest_content: Option<&str>,
    ) -> PathBuf {
        let module_dir = layout::relative_module_dir(&locked.path, &locked.version);
        let mod_content = cached_mod_content(&locked.path, ext_manifest_content);
        let package_content = cached_package_content(&mod_content);
        let release = ReleaseManifest::parse(release_manifest_content).unwrap();
        fs.add_file(module_dir.join("vo.mod"), mod_content);
        fs.add_bytes(module_dir.join("vo.package.json"), package_content);
        fs.add_file(
            module_dir.join("vo.release.json"),
            release_manifest_content.to_string(),
        );
        fs.add_file(
            module_dir.join(layout::VERSION_MARKER),
            format!("{}\n", locked.version),
        );
        fs.add_file(
            module_dir.join(layout::SOURCE_DIGEST_MARKER),
            format!("{}\n", release.source.digest),
        );
        module_dir
    }

    #[test]
    fn check_module_readiness_succeeds_for_pure_source_module() {
        let (locked, release_manifest_content) = locked_module("1.2.3", Vec::new(), None);
        let mut fs = MemoryFs::new();
        let module_dir = populate_cached_module(&mut fs, &locked, &release_manifest_content, None);

        let readiness = check_module_readiness(&fs, &locked, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(ready) => {
                assert_eq!(ready.module(), &locked.path);
                assert_eq!(ready.version(), &locked.version);
                assert_eq!(ready.module_dir(), module_dir);
                assert!(ready.artifacts().is_empty());
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_rejects_invalid_target_before_cache_io() {
        let (locked, _) = locked_module("1.2.3", Vec::new(), None);

        let readiness = check_module_readiness(&MemoryFs::new(), &locked, "INVALID");

        match readiness {
            ModuleReadiness::NotReady(ReadinessFailure::ArtifactResolutionFailed {
                error, ..
            }) => assert!(error.to_string().contains("ready-module target"), "{error}"),
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {failure}"),
            ModuleReadiness::Ready(_) => panic!("invalid target unexpectedly became ready"),
        }
    }

    #[test]
    fn check_module_readiness_succeeds_when_native_artifact_is_present_and_valid() {
        let artifact_bytes = b"native-artifact";
        let library = "libdemo.dylib";
        let extension = native_manifest("aarch64-apple-darwin");
        let artifact = published_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            library,
            artifact_bytes,
        );
        let artifact_id = artifact.id.clone();
        let (locked, release_manifest_content) =
            locked_module("1.2.3", vec![artifact], Some(&extension));
        let mut fs = MemoryFs::new();
        let module_dir = populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&extension),
        );
        fs.add_file(
            module_dir.join(crate::artifact::artifact_relative_path(&artifact_id).unwrap()),
            String::from_utf8(artifact_bytes.to_vec()).unwrap(),
        );

        let readiness = check_module_readiness(&fs, &locked, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(ready) => {
                assert_eq!(ready.artifacts().len(), 1);
                assert_eq!(ready.artifacts()[0].id().name, library);
                assert_eq!(
                    ready.ext_manifest().unwrap().manifest_path,
                    module_dir.join("vo.mod")
                );
                assert_eq!(
                    ready.artifacts()[0].cache_relative_path(),
                    crate::artifact::artifact_relative_path(&artifact_id).unwrap()
                );
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_fails_when_source_not_installed() {
        let (locked, _) = locked_module("1.2.3", Vec::new(), None);
        let fs = MemoryFs::new();

        let readiness = check_module_readiness(&fs, &locked, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(_) => panic!("expected readiness failure"),
            ModuleReadiness::NotReady(ReadinessFailure::SourceNotReady { error }) => {
                assert!(
                    error.to_string().contains("missing vo.release.json"),
                    "{}",
                    error
                );
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_reads_required_artifacts_from_installed_vo_mod() {
        let library = "libdemo.dylib";
        let extension = native_manifest("aarch64-apple-darwin");
        let artifact = published_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            library,
            b"native-artifact",
        );
        let artifact_id = artifact.id.clone();
        let (locked, release_manifest_content) =
            locked_module("1.2.3", vec![artifact], Some(&extension));
        let mut fs = MemoryFs::new();
        populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&extension),
        );

        let readiness = check_module_readiness(&fs, &locked, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(_) => panic!("expected readiness failure"),
            ModuleReadiness::NotReady(ReadinessFailure::ArtifactNotReady {
                artifact_path,
                error,
                ..
            }) => {
                assert!(artifact_path
                    .ends_with(crate::artifact::artifact_relative_path(&artifact_id).unwrap()));
                assert!(error.to_string().contains("missing"), "{}", error);
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_fails_when_declared_artifact_is_missing_from_release() {
        let extension = native_manifest("aarch64-apple-darwin");
        let (locked, release_manifest_content) =
            locked_module("1.2.3", Vec::new(), Some(&extension));
        let mut fs = MemoryFs::new();
        populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&extension),
        );

        let readiness = check_module_readiness(&fs, &locked, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(_) => panic!("expected readiness failure"),
            ModuleReadiness::NotReady(ReadinessFailure::SourceNotReady { error }) => {
                assert_eq!(
                    error.field,
                    crate::cache::validate::InstalledModuleField::ExtManifest
                );
                assert!(error.to_string().contains("missing declared artifacts"));
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_fails_when_native_target_is_unsupported() {
        let library = "libdemo.dylib";
        let extension = native_manifest("aarch64-apple-darwin");
        let (locked, release_manifest_content) = locked_module(
            "1.2.3",
            vec![published_artifact(
                "extension-native",
                "aarch64-apple-darwin",
                library,
                b"native-artifact",
            )],
            Some(&extension),
        );
        let mut fs = MemoryFs::new();
        populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&extension),
        );

        let readiness = check_module_readiness(&fs, &locked, "x86_64-unknown-linux-gnu");

        match readiness {
            ModuleReadiness::Ready(_) => panic!("expected readiness failure"),
            ModuleReadiness::NotReady(ReadinessFailure::UnsupportedNativeTarget {
                module,
                version,
                target,
                ..
            }) => {
                assert_eq!(module, "github.com/acme/demo");
                assert_eq!(version, "1.2.3");
                assert_eq!(target, "x86_64-unknown-linux-gnu");
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_succeeds_for_wasm_only_manifest_on_native_target() {
        let extension = wasm_manifest("demo_bg.wasm", "demo.js");
        let (locked, release_manifest_content) = locked_module(
            "1.2.3",
            vec![
                published_artifact(
                    "extension-wasm",
                    "wasm32-unknown-unknown",
                    "demo_bg.wasm",
                    b"wasm-artifact",
                ),
                published_artifact(
                    "extension-js-glue",
                    "wasm32-unknown-unknown",
                    "demo.js",
                    b"glue-artifact",
                ),
            ],
            Some(&extension),
        );
        let mut fs = MemoryFs::new();
        populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&extension),
        );

        let readiness = check_module_readiness(&fs, &locked, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(ready) => assert!(ready.artifacts().is_empty()),
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_project_readiness_checks_all_locked_modules() {
        let (pure_locked, pure_release_manifest_content) = locked_module("1.2.3", Vec::new(), None);
        let library = "libdemo.dylib";
        let extension = native_manifest("aarch64-apple-darwin");
        let artifact = published_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            library,
            b"native-artifact",
        );
        let artifact_id = artifact.id.clone();
        let (native_locked, native_release_manifest_content) = locked_module_for(
            "github.com/acme/native",
            "1.2.4",
            vec![artifact],
            Some(&extension),
        );
        let mut fs = MemoryFs::new();
        populate_cached_module(&mut fs, &pure_locked, &pure_release_manifest_content, None);
        let native_dir = populate_cached_module(
            &mut fs,
            &native_locked,
            &native_release_manifest_content,
            Some(&extension),
        );
        fs.add_file(
            native_dir.join(crate::artifact::artifact_relative_path(&artifact_id).unwrap()),
            "native-artifact".to_string(),
        );

        let ready = check_project_readiness(
            &fs,
            &[pure_locked.clone(), native_locked.clone()],
            "aarch64-apple-darwin",
        )
        .unwrap();

        assert_eq!(ready.len(), 2);
        assert!(ready
            .iter()
            .any(|module| module.module() == &pure_locked.path));
        assert!(ready
            .iter()
            .any(|module| module.module() == &native_locked.path && module.artifacts().len() == 1));
    }

    #[test]
    fn check_project_readiness_rejects_unsatisfied_transitive_edge_before_io() {
        let (mut importer, _) =
            locked_module_for("github.com/acme/importer", "1.0.0", Vec::new(), None);
        let (dependency, _) =
            locked_module_for("github.com/acme/dependency", "1.0.0", Vec::new(), None);
        importer.dependencies.push(LockedDependency {
            module: dependency.path.clone(),
            constraint: DepConstraint::parse("^1.1.0").unwrap(),
        });

        let mut locked_modules = vec![importer, dependency];
        locked_modules.sort_by(|left, right| left.path.cmp(&right.path));
        let error =
            check_project_readiness(&MemoryFs::new(), &locked_modules, "aarch64-apple-darwin")
                .unwrap_err();

        assert!(matches!(error, ReadinessFailure::LockedGraphInvalid { .. }));
        assert!(error
            .to_string()
            .contains("depends on github.com/acme/dependency ^1.1.0"));
        assert!(error.to_string().contains("selects 1.0.0"));
    }

    #[test]
    fn resolved_artifact_rejects_non_canonical_cache_identity() {
        let error = ResolvedArtifact::try_new(
            ArtifactId {
                kind: "extension-native".to_string(),
                target: "aarch64-apple-darwin".to_string(),
                name: "../libdemo.dylib".to_string(),
            },
            1,
            Digest::from_sha256(b"x"),
        )
        .unwrap_err();

        assert!(error.contains("artifact name"), "{error}");
    }

    #[test]
    fn resolved_artifact_rejects_empty_payloads() {
        let error = ResolvedArtifact::try_new(
            ArtifactId {
                kind: "extension-wasm".to_string(),
                target: WASM_TARGET.to_string(),
                name: "demo.wasm".to_string(),
            },
            0,
            Digest::from_sha256(b""),
        )
        .unwrap_err();
        assert!(error.contains("within 1..="), "{error}");
    }

    #[test]
    fn resolved_artifact_accepts_the_size_ceiling_and_rejects_the_next_byte() {
        let id = ArtifactId {
            kind: "extension-wasm".to_string(),
            target: WASM_TARGET.to_string(),
            name: "demo.wasm".to_string(),
        };
        ResolvedArtifact::try_new(
            id.clone(),
            crate::MAX_MODULE_ARTIFACT_BYTES,
            Digest::from_sha256(b"metadata-only-boundary"),
        )
        .unwrap();

        let error = ResolvedArtifact::try_new(
            id,
            crate::MAX_MODULE_ARTIFACT_BYTES.checked_add(1).unwrap(),
            Digest::from_sha256(b"metadata-only-overflow"),
        )
        .unwrap_err();
        assert!(error.contains("within 1..=268435456"), "{error}");
    }

    #[test]
    fn ready_module_rejects_wrong_target_and_duplicate_kind() {
        let wrong_target_manifest = parse_manifest(
            r#"
[extension]
name = "demo"

[extension.native]
targets = ["aarch64-apple-darwin", "x86_64-unknown-linux-gnu"]
"#,
        );
        let wrong_target = ResolvedArtifact::try_new(
            ArtifactId {
                kind: "extension-native".to_string(),
                target: "x86_64-unknown-linux-gnu".to_string(),
                name: "libdemo.so".to_string(),
            },
            1,
            Digest::from_sha256(b"x"),
        )
        .unwrap();
        let error = ReadyModule::try_new(
            ModulePath::parse("github.com/acme/demo").unwrap(),
            ExactVersion::parse("1.2.3").unwrap(),
            "aarch64-apple-darwin",
            vec![wrong_target],
            Some(wrong_target_manifest),
        )
        .unwrap_err();
        assert!(error.contains("while the ready module targets"), "{error}");

        let manifest = parse_manifest(&wasm_manifest("demo.wasm", "demo.js"));
        let first = ResolvedArtifact::try_new(
            ArtifactId {
                kind: "extension-wasm".to_string(),
                target: WASM_TARGET.to_string(),
                name: "demo.wasm".to_string(),
            },
            1,
            Digest::from_sha256(b"a"),
        )
        .unwrap();
        let second = ResolvedArtifact::try_new(
            ArtifactId {
                kind: "extension-wasm".to_string(),
                target: WASM_TARGET.to_string(),
                name: "other.wasm".to_string(),
            },
            1,
            Digest::from_sha256(b"b"),
        )
        .unwrap();
        let error = ReadyModule::try_new(
            ModulePath::parse("github.com/acme/demo").unwrap(),
            ExactVersion::parse("1.2.3").unwrap(),
            WASM_TARGET,
            vec![first, second],
            Some(manifest),
        )
        .unwrap_err();
        assert!(error.contains("duplicate extension-wasm"), "{error}");
    }

    #[test]
    fn ready_module_derives_its_canonical_cache_directory() {
        let ready = ReadyModule::try_new(
            ModulePath::parse("github.com/acme/demo").unwrap(),
            ExactVersion::parse("1.2.3").unwrap(),
            "aarch64-apple-darwin",
            Vec::new(),
            None,
        )
        .unwrap();

        assert_eq!(
            ready.module_dir(),
            layout::relative_module_dir(ready.module(), ready.version())
        );
    }

    #[test]
    fn ready_module_rejects_a_version_incompatible_with_its_module_path() {
        let error = ReadyModule::try_new(
            ModulePath::parse("github.com/acme/demo/v2").unwrap(),
            ExactVersion::parse("1.2.3").unwrap(),
            "aarch64-apple-darwin",
            Vec::new(),
            None,
        )
        .unwrap_err();

        assert!(error.contains("does not accept version"), "{error}");
    }

    #[test]
    fn ready_module_revalidates_forged_extension_metadata() {
        let valid = parse_manifest(
            r#"
[extension]
name = "demo"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "js/renderer.js"
"#,
        );

        let mut traversal = valid.clone();
        traversal
            .web
            .as_mut()
            .unwrap()
            .js_modules
            .insert("renderer".to_string(), "../../outside.js".to_string());
        let error = ReadyModule::try_new(
            ModulePath::parse("github.com/acme/demo").unwrap(),
            ExactVersion::parse("1.2.3").unwrap(),
            "aarch64-apple-darwin",
            Vec::new(),
            Some(traversal),
        )
        .unwrap_err();
        assert!(error.contains("normalized module-relative path"), "{error}");

        let mut oversized = valid;
        oversized.web.as_mut().unwrap().capabilities =
            vec!["widget".to_string(); crate::MAX_MODULE_METADATA_ENTRIES + 1];
        let error = ReadyModule::try_new(
            ModulePath::parse("github.com/acme/demo").unwrap(),
            ExactVersion::parse("1.2.3").unwrap(),
            "aarch64-apple-darwin",
            Vec::new(),
            Some(oversized),
        )
        .unwrap_err();
        assert!(error.contains("contains more than"), "{error}");
    }
}
