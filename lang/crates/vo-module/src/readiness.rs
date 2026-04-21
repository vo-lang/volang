use std::collections::BTreeMap;
use std::fmt;
use std::path::{Path, PathBuf};

use vo_common::vfs::FileSystem;

use crate::artifact::required_artifacts_for_target;
use crate::cache::layout;
use crate::cache::validate::{
    validate_installed_artifact, validate_installed_module, InstalledModuleError,
};
use crate::digest::Digest;
use crate::ext_manifest::{parse_ext_manifest_content, ExtensionManifest};
use crate::identity::{ArtifactId, ModulePath};
use crate::schema::lockfile::LockedModule;
use crate::version::ExactVersion;
use crate::Error;

const WASM_TARGET: &str = "wasm32-unknown-unknown";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedArtifact {
    pub id: ArtifactId,
    pub cache_relative_path: PathBuf,
    pub size: u64,
    pub digest: Digest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadyModule {
    pub module: ModulePath,
    pub version: ExactVersion,
    pub module_dir: PathBuf,
    pub artifacts: Vec<ResolvedArtifact>,
    pub ext_manifest: Option<ExtensionManifest>,
}

#[derive(Debug)]
pub enum ModuleReadiness {
    Ready(Box<ReadyModule>),
    NotReady(ReadinessFailure),
}

#[derive(Debug)]
pub enum ReadinessFailure {
    SourceNotReady {
        error: Box<InstalledModuleError>,
    },
    ExtensionManifestReadFailed {
        module: String,
        version: String,
        manifest_path: PathBuf,
        detail: String,
    },
    ExtensionManifestParseFailed {
        module: String,
        version: String,
        manifest_path: PathBuf,
        detail: String,
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
    ext_manifest: Option<&ExtensionManifest>,
    target: &str,
) -> ModuleReadiness {
    let module_dir = layout::relative_module_dir(locked.path.as_str(), &locked.version);
    if let Err(error) = validate_installed_module(fs, &module_dir, locked) {
        return ModuleReadiness::NotReady(ReadinessFailure::SourceNotReady {
            error: Box::new(error),
        });
    }

    let Some(ext_manifest) = ext_manifest else {
        return ModuleReadiness::Ready(Box::new(ready_module(
            locked,
            module_dir,
            Vec::new(),
            None,
        )));
    };

    if requires_declared_native_target(ext_manifest, target) {
        return ModuleReadiness::NotReady(ReadinessFailure::UnsupportedNativeTarget {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            target: target.to_string(),
            manifest_path: ext_manifest.manifest_path.clone(),
        });
    }

    let required = match required_artifacts_for_target(locked, Some(ext_manifest), target) {
        Ok(required) => required,
        Err(error) => {
            return ModuleReadiness::NotReady(ReadinessFailure::ArtifactResolutionFailed {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                manifest_path: ext_manifest.manifest_path.clone(),
                error: Box::new(error),
            });
        }
    };

    let mut artifacts = Vec::with_capacity(required.len());
    for required_artifact in required {
        let artifact_path = module_dir.join(&required_artifact.cache_relative_path);
        if let Err(error) = validate_installed_artifact(
            fs,
            &artifact_path,
            locked,
            required_artifact.locked_artifact,
        ) {
            return ModuleReadiness::NotReady(ReadinessFailure::ArtifactNotReady {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                artifact_path: scoped_path(fs, &artifact_path),
                error: Box::new(error),
            });
        }
        artifacts.push(ResolvedArtifact {
            id: required_artifact.locked_artifact.id.clone(),
            cache_relative_path: required_artifact.cache_relative_path.clone(),
            size: required_artifact.locked_artifact.size,
            digest: required_artifact.locked_artifact.digest.clone(),
        });
    }

    ModuleReadiness::Ready(Box::new(ready_module(
        locked,
        module_dir,
        artifacts,
        Some(ext_manifest.clone()),
    )))
}

pub fn check_project_readiness<F: FileSystem>(
    fs: &F,
    locked_modules: &[LockedModule],
    target: &str,
) -> Result<Vec<ReadyModule>, ReadinessFailure> {
    let mut manifests = BTreeMap::new();
    for locked in locked_modules {
        let module_dir = layout::relative_module_dir(locked.path.as_str(), &locked.version);
        let manifest_rel = module_dir.join("vo.ext.toml");
        if !fs.exists(&manifest_rel) {
            continue;
        }
        let manifest_path = scoped_path(fs, &manifest_rel);
        let content = fs.read_file(&manifest_rel).map_err(|error| {
            ReadinessFailure::ExtensionManifestReadFailed {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                manifest_path: manifest_path.clone(),
                detail: error.to_string(),
            }
        })?;
        let manifest = parse_ext_manifest_content(&content, &manifest_path).map_err(|error| {
            ReadinessFailure::ExtensionManifestParseFailed {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                manifest_path: manifest_path.clone(),
                detail: error.to_string(),
            }
        })?;
        manifests.insert(module_dir, manifest);
    }

    let mut ready = Vec::with_capacity(locked_modules.len());
    for locked in locked_modules {
        let module_dir = layout::relative_module_dir(locked.path.as_str(), &locked.version);
        match check_module_readiness(fs, locked, manifests.get(&module_dir), target) {
            ModuleReadiness::Ready(module) => ready.push(*module),
            ModuleReadiness::NotReady(failure) => return Err(failure),
        }
    }
    Ok(ready)
}

fn ready_module(
    locked: &LockedModule,
    module_dir: PathBuf,
    artifacts: Vec<ResolvedArtifact>,
    ext_manifest: Option<ExtensionManifest>,
) -> ReadyModule {
    ReadyModule {
        module: locked.path.clone(),
        version: locked.version.clone(),
        module_dir,
        artifacts,
        ext_manifest,
    }
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
            Self::SourceNotReady { error } => write!(f, "{}", error),
            Self::ExtensionManifestReadFailed {
                module,
                version,
                manifest_path,
                detail,
            } => {
                write!(
                    f,
                    "failed to read cached extension manifest {} for {}@{}: {}",
                    manifest_path.display(),
                    module,
                    version,
                    detail,
                )
            }
            Self::ExtensionManifestParseFailed {
                module,
                version,
                manifest_path,
                detail,
            } => {
                write!(
                    f,
                    "failed to parse cached extension manifest {} for {}@{}: {}",
                    manifest_path.display(),
                    module,
                    version,
                    detail,
                )
            }
            Self::UnsupportedNativeTarget {
                module,
                version,
                target,
                ..
            } => {
                write!(
                    f,
                    "vo.ext.toml does not declare extension-native support for target {} in {}@{}",
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
    use crate::schema::lockfile::{LockedArtifact, LockedModule};
    use crate::schema::manifest::{ManifestArtifact, ManifestSource, ReleaseManifest};
    use crate::version::ExactVersion;
    use crate::version::ToolchainConstraint;
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

    fn locked_module(version: &str, artifacts: Vec<LockedArtifact>) -> (LockedModule, String) {
        let manifest = ReleaseManifest {
            schema_version: 1,
            module: ModulePath::parse("github.com/acme/demo").unwrap(),
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
        let release_manifest_content = format!("{}\n", manifest.render());
        let locked = crate::lock::locked_module_from_manifest_raw(
            &manifest,
            release_manifest_content.as_bytes(),
        );
        (locked, release_manifest_content)
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
        let module_dir = layout::relative_module_dir(locked.path.as_str(), &locked.version);
        fs.add_file(
            module_dir.join("vo.mod"),
            format!("module {}\nvo {}\n", locked.path, locked.vo),
        );
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
        if let Some(ext_manifest_content) = ext_manifest_content {
            fs.add_file(
                module_dir.join("vo.ext.toml"),
                ext_manifest_content.to_string(),
            );
        }
        module_dir
    }

    #[test]
    fn check_module_readiness_succeeds_for_pure_source_module() {
        let (locked, release_manifest_content) = locked_module("v1.2.3", Vec::new());
        let mut fs = MemoryFs::new();
        let module_dir = populate_cached_module(&mut fs, &locked, &release_manifest_content, None);

        let readiness = check_module_readiness(&fs, &locked, None, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(ready) => {
                assert_eq!(ready.module, locked.path);
                assert_eq!(ready.version, locked.version);
                assert_eq!(ready.module_dir, module_dir);
                assert!(ready.artifacts.is_empty());
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_succeeds_when_native_artifact_is_present_and_valid() {
        let artifact_bytes = b"native-artifact";
        let library = "libdemo.dylib";
        let (locked, release_manifest_content) = locked_module(
            "v1.2.3",
            vec![locked_artifact(
                "extension-native",
                "aarch64-apple-darwin",
                library,
                artifact_bytes,
            )],
        );
        let manifest = parse_ext_manifest_content(
            &native_manifest("aarch64-apple-darwin", library),
            Path::new("vo.ext.toml"),
        )
        .unwrap();
        let mut fs = MemoryFs::new();
        let module_dir = populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&native_manifest("aarch64-apple-darwin", library)),
        );
        fs.add_file(
            module_dir.join("artifacts").join(library),
            String::from_utf8(artifact_bytes.to_vec()).unwrap(),
        );

        let readiness =
            check_module_readiness(&fs, &locked, Some(&manifest), "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(ready) => {
                assert_eq!(ready.artifacts.len(), 1);
                assert_eq!(ready.artifacts[0].id.name, library);
                assert_eq!(
                    ready.artifacts[0].cache_relative_path,
                    Path::new("artifacts").join(library)
                );
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_fails_when_source_not_installed() {
        let (locked, _) = locked_module("v1.2.3", Vec::new());
        let fs = MemoryFs::new();

        let readiness = check_module_readiness(&fs, &locked, None, "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(_) => panic!("expected readiness failure"),
            ModuleReadiness::NotReady(ReadinessFailure::SourceNotReady { error }) => {
                assert!(error.to_string().contains("missing directory"), "{}", error);
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_fails_when_artifact_missing_from_cache() {
        let library = "libdemo.dylib";
        let (locked, release_manifest_content) = locked_module(
            "v1.2.3",
            vec![locked_artifact(
                "extension-native",
                "aarch64-apple-darwin",
                library,
                b"native-artifact",
            )],
        );
        let manifest = parse_ext_manifest_content(
            &native_manifest("aarch64-apple-darwin", library),
            Path::new("vo.ext.toml"),
        )
        .unwrap();
        let mut fs = MemoryFs::new();
        populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&native_manifest("aarch64-apple-darwin", library)),
        );

        let readiness =
            check_module_readiness(&fs, &locked, Some(&manifest), "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(_) => panic!("expected readiness failure"),
            ModuleReadiness::NotReady(ReadinessFailure::ArtifactNotReady {
                artifact_path,
                error,
                ..
            }) => {
                assert!(artifact_path.ends_with(Path::new("artifacts").join(library)));
                assert!(error.to_string().contains("not in cache"), "{}", error);
            }
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_module_readiness_fails_when_declared_artifact_is_missing_from_lock() {
        let library = "libdemo.dylib";
        let (locked, release_manifest_content) = locked_module("v1.2.3", Vec::new());
        let manifest = parse_ext_manifest_content(
            &native_manifest("aarch64-apple-darwin", library),
            Path::new("vo.ext.toml"),
        )
        .unwrap();
        let mut fs = MemoryFs::new();
        populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&native_manifest("aarch64-apple-darwin", library)),
        );

        let readiness =
            check_module_readiness(&fs, &locked, Some(&manifest), "aarch64-apple-darwin");

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
        let (locked, release_manifest_content) = locked_module(
            "v1.2.3",
            vec![locked_artifact(
                "extension-native",
                "aarch64-apple-darwin",
                library,
                b"native-artifact",
            )],
        );
        let manifest = parse_ext_manifest_content(
            &native_manifest("aarch64-apple-darwin", library),
            Path::new("vo.ext.toml"),
        )
        .unwrap();
        let mut fs = MemoryFs::new();
        populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&native_manifest("aarch64-apple-darwin", library)),
        );

        let readiness =
            check_module_readiness(&fs, &locked, Some(&manifest), "x86_64-unknown-linux-gnu");

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
        );
        let manifest = parse_ext_manifest_content(
            &wasm_manifest("demo_bg.wasm", "demo.js"),
            Path::new("vo.ext.toml"),
        )
        .unwrap();
        let mut fs = MemoryFs::new();
        populate_cached_module(
            &mut fs,
            &locked,
            &release_manifest_content,
            Some(&wasm_manifest("demo_bg.wasm", "demo.js")),
        );

        let readiness =
            check_module_readiness(&fs, &locked, Some(&manifest), "aarch64-apple-darwin");

        match readiness {
            ModuleReadiness::Ready(ready) => assert!(ready.artifacts.is_empty()),
            ModuleReadiness::NotReady(failure) => panic!("unexpected failure: {}", failure),
        }
    }

    #[test]
    fn check_project_readiness_checks_all_locked_modules() {
        let (pure_locked, pure_release_manifest_content) = locked_module("v1.2.3", Vec::new());
        let library = "libdemo.dylib";
        let (native_locked, native_release_manifest_content) = locked_module(
            "v1.2.4",
            vec![locked_artifact(
                "extension-native",
                "aarch64-apple-darwin",
                library,
                b"native-artifact",
            )],
        );
        let mut fs = MemoryFs::new();
        populate_cached_module(&mut fs, &pure_locked, &pure_release_manifest_content, None);
        let native_dir = populate_cached_module(
            &mut fs,
            &native_locked,
            &native_release_manifest_content,
            Some(&native_manifest("aarch64-apple-darwin", library)),
        );
        fs.add_file(
            native_dir.join("artifacts").join(library),
            "native-artifact".to_string(),
        );

        let ready = check_project_readiness(
            &fs,
            &[pure_locked.clone(), native_locked.clone()],
            "aarch64-apple-darwin",
        )
        .unwrap();

        assert_eq!(ready.len(), 2);
        assert!(ready.iter().any(|module| module.module == pure_locked.path));
        assert!(ready
            .iter()
            .any(|module| { module.module == native_locked.path && module.artifacts.len() == 1 }));
    }
}
