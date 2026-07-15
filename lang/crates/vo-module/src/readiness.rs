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
        if size > crate::MAX_MODULE_ARTIFACT_BYTES {
            return Err(format!(
                "artifact {} size {} exceeds the {}-byte limit",
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
    ids.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
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
    let ext_manifest = match validate_installed_module_with_metadata(fs, &module_dir, locked) {
        Ok(manifest) => manifest,
        Err(error) => {
            return ModuleReadiness::NotReady(ReadinessFailure::SourceNotReady {
                error: Box::new(error),
            });
        }
    };

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

    let required = match required_artifacts_for_target(locked, ext_manifest.as_ref(), target) {
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
        if let Err(error) = validate_installed_artifact(
            fs,
            &module_dir,
            locked,
            &required_artifact.locked_artifact.id,
        ) {
            return ModuleReadiness::NotReady(ReadinessFailure::ArtifactNotReady {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                artifact_path: scoped_path(fs, &artifact_path),
                error: Box::new(error),
            });
        }
        let resolved = match ResolvedArtifact::try_new(
            required_artifact.locked_artifact.id.clone(),
            required_artifact.locked_artifact.size,
            required_artifact.locked_artifact.digest.clone(),
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
/// Workspace overrides may remove registry modules from the materialized set,
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
    let mut ready = Vec::with_capacity(locked_modules.len());
    for locked in locked_modules {
        match check_module_readiness(fs, locked, target) {
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
    use crate::schema::lockfile::{LockedArtifact, LockedModule, LockedRequirement};
    use crate::schema::manifest::{
        ManifestArtifact, ManifestSource, ManifestWebManifest, ReleaseManifest,
    };
    use crate::version::ToolchainConstraint;
    use crate::version::{DepConstraint, ExactVersion};
    use vo_common::vfs::MemoryFs;

    fn locked_artifact(kind: &str, target: &str, name: &str, bytes: &[u8]) -> LockedArtifact {
        LockedArtifact {
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
        let mut content = format!("module {module}\nvo ^0.1.0\n");
        if let Some(extension) = ext_manifest_content {
            content.push('\n');
            content.push_str(extension);
        }
        content
    }

    fn locked_module(
        version: &str,
        artifacts: Vec<LockedArtifact>,
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
        artifacts: Vec<LockedArtifact>,
        ext_manifest_content: Option<&str>,
    ) -> (LockedModule, String) {
        let module = ModulePath::parse(module).unwrap();
        let mod_content = cached_mod_content(&module, ext_manifest_content);
        let source_set =
            crate::schema::canonical_source_file_set(&[crate::schema::SourceFileEntry {
                path: "vo.mod".to_string(),
                size: mod_content.len() as u64,
                digest: Digest::from_sha256(mod_content.as_bytes()),
            }])
            .unwrap();
        let web_manifest_content =
            cached_web_manifest_content(version, &artifacts, &mod_content, &source_set.digest);
        let manifest = ReleaseManifest {
            schema_version: 1,
            module,
            version: ExactVersion::parse(version).unwrap(),
            commit: "1111111111111111111111111111111111111111".to_string(),
            module_root: ".".to_string(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            require: Vec::new(),
            source: ManifestSource {
                name: format!("demo-{}-source.tar.gz", version),
                size: 3,
                digest: Digest::parse(
                    "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                )
                .unwrap(),
                files_size: source_set.total_size,
                files_digest: source_set.digest,
            },
            web_manifest: ManifestWebManifest {
                size: web_manifest_content.len() as u64,
                digest: Digest::from_sha256(web_manifest_content.as_bytes()),
            },
            artifacts: artifacts
                .iter()
                .map(|artifact| ManifestArtifact {
                    id: artifact.id.clone(),
                    size: artifact.size,
                    digest: artifact.digest.clone(),
                })
                .collect(),
        };
        let release_manifest_content = format!("{}\n", manifest.render().unwrap());
        let locked = crate::lock::locked_module_from_manifest_raw(
            &manifest,
            release_manifest_content.as_bytes(),
        );
        (locked, release_manifest_content)
    }

    fn cached_web_manifest_content(
        version: &str,
        artifacts: &[LockedArtifact],
        mod_content: &str,
        source_digest: &Digest,
    ) -> String {
        let mod_file = crate::schema::modfile::ModFile::parse(mod_content).unwrap();
        let extension = mod_file.extension.as_ref().map(|extension| {
            serde_json::json!({
                "name": extension.name,
                "include": extension.include,
                "wasm": extension.wasm,
                "web": extension.web,
            })
        });
        let mut artifacts = artifacts
            .iter()
            .filter(|artifact| artifact.id.kind != "extension-native")
            .map(|artifact| {
                serde_json::json!({
                    "kind": artifact.id.kind,
                    "target": artifact.id.target,
                    "name": artifact.id.name,
                    "path": artifact.id.name,
                    "size": artifact.size,
                    "digest": artifact.digest,
                })
            })
            .collect::<Vec<_>>();
        artifacts.sort_by(|left, right| {
            let left = (
                left["kind"].as_str(),
                left["target"].as_str(),
                left["name"].as_str(),
            );
            let right = (
                right["kind"].as_str(),
                right["target"].as_str(),
                right["name"].as_str(),
            );
            left.cmp(&right)
        });
        format!(
            "{}\n",
            serde_json::to_string_pretty(&serde_json::json!({
                "schema_version": 1,
                "module": mod_file.module.as_str(),
                "version": version,
                "commit": "1111111111111111111111111111111111111111",
                "module_root": ".",
                "vo": "^0.1.0",
                "require": [],
                "source_digest": source_digest,
                "source": [{
                    "path": "vo.mod",
                    "size": mod_content.len() as u64,
                    "digest": Digest::from_sha256(mod_content.as_bytes()),
                }],
                "web": mod_file.web,
                "extension": extension,
                "artifacts": artifacts,
            }))
            .unwrap()
        )
    }

    fn native_manifest(target: &str, library: &str) -> String {
        format!(
            r#"
[extension]
name = "demo"

[extension.native]
path = "rust/target/{{profile}}/libdemo"

[[extension.native.targets]]
target = "{target}"
library = "{library}"
"#,
        )
    }

    fn wasm_manifest(wasm: &str, js_glue: &str) -> String {
        format!(
            r#"
[extension]
name = "demo"

[extension.wasm]
type = "bindgen"
wasm = "{wasm}"
js_glue = "{js_glue}"
"#,
        )
    }

    fn populate_cached_module(
        fs: &mut MemoryFs,
        locked: &LockedModule,
        release_manifest_content: &str,
        ext_manifest_content: Option<&str>,
    ) -> PathBuf {
        let module_dir = layout::relative_module_dir(&locked.path, &locked.version);
        let mod_content = cached_mod_content(&locked.path, ext_manifest_content);
        let source_set =
            crate::schema::canonical_source_file_set(&[crate::schema::SourceFileEntry {
                path: "vo.mod".to_string(),
                size: mod_content.len() as u64,
                digest: Digest::from_sha256(mod_content.as_bytes()),
            }])
            .unwrap();
        let web_content = cached_web_manifest_content(
            &locked.version.to_string(),
            &locked.artifacts,
            &mod_content,
            &source_set.digest,
        );
        fs.add_file(module_dir.join("vo.mod"), mod_content);
        fs.add_file(module_dir.join("vo.web.json"), web_content);
        fs.add_file(
            module_dir.join(".vo-version"),
            format!("{}\n", locked.version),
        );
        fs.add_file(
            module_dir.join(".vo-source-digest"),
            format!("{}\n", locked.source),
        );
        fs.add_file(
            module_dir.join("vo.release.json"),
            release_manifest_content.to_string(),
        );
        module_dir
    }

    #[test]
    fn check_module_readiness_succeeds_for_pure_source_module() {
        let (locked, release_manifest_content) = locked_module("v1.2.3", Vec::new(), None);
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
        let (locked, _) = locked_module("v1.2.3", Vec::new(), None);

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
        let extension = native_manifest("aarch64-apple-darwin", library);
        let (locked, release_manifest_content) = locked_module(
            "v1.2.3",
            vec![locked_artifact(
                "extension-native",
                "aarch64-apple-darwin",
                library,
                artifact_bytes,
            )],
            Some(&extension),
        );
        let mut fs = MemoryFs::new();
        let module_dir = populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&extension),
        );
        fs.add_file(
            module_dir
                .join(crate::artifact::artifact_relative_path(&locked.artifacts[0].id).unwrap()),
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
                    crate::artifact::artifact_relative_path(&locked.artifacts[0].id).unwrap()
                );
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_fails_when_source_not_installed() {
        let (locked, _) = locked_module("v1.2.3", Vec::new(), None);
        let fs = MemoryFs::new();

        let readiness = check_module_readiness(&fs, &locked, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(_) => panic!("expected readiness failure"),
            ModuleReadiness::NotReady(ReadinessFailure::SourceNotReady { error }) => {
                assert!(error.to_string().contains("missing directory"), "{}", error);
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_reads_required_artifacts_from_installed_vo_mod() {
        let library = "libdemo.dylib";
        let extension = native_manifest("aarch64-apple-darwin", library);
        let (locked, release_manifest_content) = locked_module(
            "v1.2.3",
            vec![locked_artifact(
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

        let readiness = check_module_readiness(&fs, &locked, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(_) => panic!("expected readiness failure"),
            ModuleReadiness::NotReady(ReadinessFailure::ArtifactNotReady {
                artifact_path,
                error,
                ..
            }) => {
                assert!(artifact_path.ends_with(
                    crate::artifact::artifact_relative_path(&locked.artifacts[0].id).unwrap()
                ));
                assert!(error.to_string().contains("missing"), "{}", error);
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_fails_when_declared_artifact_is_missing_from_lock() {
        let library = "libdemo.dylib";
        let extension = native_manifest("aarch64-apple-darwin", library);
        let (locked, release_manifest_content) =
            locked_module("v1.2.3", Vec::new(), Some(&extension));
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
        let extension = native_manifest("aarch64-apple-darwin", library);
        let (locked, release_manifest_content) = locked_module(
            "v1.2.3",
            vec![locked_artifact(
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
                assert_eq!(version, "v1.2.3");
                assert_eq!(target, "x86_64-unknown-linux-gnu");
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_succeeds_for_wasm_only_manifest_on_native_target() {
        let extension = wasm_manifest("demo_bg.wasm", "demo.js");
        let (locked, release_manifest_content) = locked_module(
            "v1.2.3",
            vec![
                locked_artifact(
                    "extension-wasm",
                    "wasm32-unknown-unknown",
                    "demo_bg.wasm",
                    b"wasm-artifact",
                ),
                locked_artifact(
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
        let (pure_locked, pure_release_manifest_content) =
            locked_module("v1.2.3", Vec::new(), None);
        let library = "libdemo.dylib";
        let extension = native_manifest("aarch64-apple-darwin", library);
        let (native_locked, native_release_manifest_content) = locked_module_for(
            "github.com/acme/native",
            "v1.2.4",
            vec![locked_artifact(
                "extension-native",
                "aarch64-apple-darwin",
                library,
                b"native-artifact",
            )],
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
            native_dir.join(
                crate::artifact::artifact_relative_path(&native_locked.artifacts[0].id).unwrap(),
            ),
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
            locked_module_for("github.com/acme/importer", "v1.0.0", Vec::new(), None);
        let (dependency, _) =
            locked_module_for("github.com/acme/dependency", "v1.0.0", Vec::new(), None);
        importer.deps.push(LockedRequirement {
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
            .contains("requires github.com/acme/dependency ^1.1.0"));
        assert!(error.to_string().contains("selects v1.0.0"));
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
    fn ready_module_rejects_wrong_target_and_duplicate_kind() {
        let wrong_target_manifest = parse_ext_manifest_content(
            r#"
[extension]
name = "demo"

[extension.native]
path = "rust/target/{profile}/libdemo"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libdemo.dylib"

[[extension.native.targets]]
target = "x86_64-unknown-linux-gnu"
library = "libdemo.so"
"#,
            Path::new("vo.mod"),
        )
        .unwrap();
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
            ExactVersion::parse("v1.2.3").unwrap(),
            "aarch64-apple-darwin",
            vec![wrong_target],
            Some(wrong_target_manifest),
        )
        .unwrap_err();
        assert!(error.contains("while the ready module targets"), "{error}");

        let manifest =
            parse_ext_manifest_content(&wasm_manifest("demo.wasm", "demo.js"), Path::new("vo.mod"))
                .unwrap();
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
            ExactVersion::parse("v1.2.3").unwrap(),
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
            ExactVersion::parse("v1.2.3").unwrap(),
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
            ExactVersion::parse("v1.2.3").unwrap(),
            "aarch64-apple-darwin",
            Vec::new(),
            None,
        )
        .unwrap_err();

        assert!(error.contains("does not accept version"), "{error}");
    }

    #[test]
    fn ready_module_revalidates_forged_extension_metadata() {
        let valid = parse_ext_manifest_content(
            r#"
[extension]
name = "demo"

[extension.web]
capabilities = ["widget"]

[extension.web.js]
renderer = "js/renderer.js"
"#,
            Path::new("vo.mod"),
        )
        .unwrap();

        let mut traversal = valid.clone();
        traversal
            .web
            .as_mut()
            .unwrap()
            .js_modules
            .insert("renderer".to_string(), "../../outside.js".to_string());
        let error = ReadyModule::try_new(
            ModulePath::parse("github.com/acme/demo").unwrap(),
            ExactVersion::parse("v1.2.3").unwrap(),
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
            ExactVersion::parse("v1.2.3").unwrap(),
            "aarch64-apple-darwin",
            Vec::new(),
            Some(oversized),
        )
        .unwrap_err();
        assert!(error.contains("contains more than"), "{error}");
    }
}
