//! FileSystem-generic validation of installed modules against lock metadata.
//!
//! This is the **single source of truth** for "does an installed module match
//! its `LockedModule`?".  All consumers — `vo-engine` (native), `vo-web` (VFS),
//! and `vo-module::materialize` (download skip-check) — call into this module
//! instead of reimplementing the validation logic.

use std::fmt;
use std::path::Path;

use vo_common::vfs::FileSystem;

use crate::digest::Digest;
use crate::identity::ArtifactId;
use crate::schema::lockfile::LockedModule;
use crate::schema::manifest::{ManifestArtifact, ManifestDependency, ReleaseManifest};
use crate::schema::PackageManifest;

// ── Error types ──────────────────────────────────────────────────────────────

/// Structured error for installed-module validation failures.
///
/// Carries enough metadata for both CLI error messages and IDE diagnostics.
#[derive(Debug)]
pub struct InstalledModuleError {
    pub module: String,
    pub version: String,
    pub field: InstalledModuleField,
    pub kind: Box<InstalledModuleErrorKind>,
}

/// Which part of the installed module failed validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstalledModuleField {
    Directory,
    VersionMarker,
    SourceDigestMarker,
    ModFile,
    SourceFiles,
    ReleaseManifest,
    PackageManifest,
    ExtManifest,
    Artifact,
}

/// What went wrong.
#[derive(Debug)]
pub enum InstalledModuleErrorKind {
    Missing {
        detail: String,
    },
    Mismatch {
        expected: String,
        found: String,
    },
    ParseFailed {
        detail: String,
    },
    ValidationFailed {
        detail: String,
    },
    LockedModuleMismatch {
        field: String,
        expected: String,
        found: String,
    },
}

impl fmt::Display for InstalledModuleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let field = match self.field {
            InstalledModuleField::Directory => "directory",
            InstalledModuleField::VersionMarker => ".vo-version",
            InstalledModuleField::SourceDigestMarker => ".vo-source-digest",
            InstalledModuleField::ModFile => "vo.mod",
            InstalledModuleField::SourceFiles => "published source file set",
            InstalledModuleField::ReleaseManifest => "vo.release.json",
            InstalledModuleField::PackageManifest => "vo.package.json",
            InstalledModuleField::ExtManifest => "vo.mod metadata",
            InstalledModuleField::Artifact => "artifact",
        };
        match self.kind.as_ref() {
            InstalledModuleErrorKind::Missing { detail } => {
                write!(
                    f,
                    "installed module {}@{} is missing {}: {}",
                    self.module, self.version, field, detail,
                )
            }
            InstalledModuleErrorKind::Mismatch { expected, found } => {
                write!(
                    f,
                    "installed module {}@{} {} mismatch: expected {}, found {}",
                    self.module, self.version, field, expected, found,
                )
            }
            InstalledModuleErrorKind::ParseFailed { detail } => {
                write!(
                    f,
                    "installed module {}@{} {} parse error: {}",
                    self.module, self.version, field, detail,
                )
            }
            InstalledModuleErrorKind::ValidationFailed { detail } => {
                write!(
                    f,
                    "installed module {}@{} {} validation failed: {}",
                    self.module, self.version, field, detail,
                )
            }
            InstalledModuleErrorKind::LockedModuleMismatch {
                field,
                expected,
                found,
            } => {
                write!(
                    f,
                    "vo.lock entry for {} does not match published release manifest: {}: expected {}, found {}",
                    self.module, field, expected, found,
                )
            }
        }
    }
}

impl std::error::Error for InstalledModuleError {}

impl fmt::Display for InstalledModuleErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Missing { detail } => write!(f, "{}", detail),
            Self::Mismatch { expected, found } => {
                write!(f, "expected {}, found {}", expected, found)
            }
            Self::ParseFailed { detail } => write!(f, "{}", detail),
            Self::ValidationFailed { detail } => write!(f, "{}", detail),
            Self::LockedModuleMismatch {
                field,
                expected,
                found,
            } => write!(f, "{}: expected {}, found {}", field, expected, found,),
        }
    }
}

// ── Validation ───────────────────────────────────────────────────────────────

fn parse_installed_release_manifest<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
    module: &str,
    version: &str,
) -> Result<(crate::schema::manifest::ReleaseManifest, Digest, usize), InstalledModuleError> {
    let manifest_path = module_dir.join("vo.release.json");
    let kind = super::source_integrity::entry_kind(
        fs,
        &manifest_path,
        locked,
        InstalledModuleField::ReleaseManifest,
    )?;
    super::source_integrity::require_regular_file(
        kind,
        &manifest_path,
        locked,
        InstalledModuleField::ReleaseManifest,
    )?;
    let manifest_bytes = fs
        .read_bytes_limited(&manifest_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(|error| InstalledModuleError {
            module: module.to_string(),
            version: version.to_string(),
            field: InstalledModuleField::ReleaseManifest,
            kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
                detail: format!("cannot read cached vo.release.json: {error}"),
            }),
        })?;
    let manifest_digest = Digest::from_sha256(&manifest_bytes);
    if manifest_digest != locked.release {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.to_string(),
            field: InstalledModuleField::ReleaseManifest,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: locked.release.as_str().to_string(),
                found: manifest_digest.as_str().to_string(),
            }),
        });
    }
    let manifest_content =
        std::str::from_utf8(&manifest_bytes).map_err(|error| InstalledModuleError {
            module: module.to_string(),
            version: version.to_string(),
            field: InstalledModuleField::ReleaseManifest,
            kind: Box::new(InstalledModuleErrorKind::ParseFailed {
                detail: format!("cached vo.release.json is not valid UTF-8: {}", error),
            }),
        })?;
    let manifest = crate::registry::parse_requested_release_manifest(
        manifest_content,
        &locked.path,
        &locked.version,
    )
    .map_err(|error| InstalledModuleError {
        module: module.to_string(),
        version: version.to_string(),
        field: InstalledModuleField::ReleaseManifest,
        kind: Box::new(InstalledModuleErrorKind::ParseFailed {
            detail: error.to_string(),
        }),
    })?;
    Ok((manifest, manifest_digest, manifest_bytes.len()))
}

pub(crate) fn canonical_mod_dependencies(
    dependencies: &[crate::schema::modfile::Dependency],
) -> Vec<(String, String)> {
    let mut canonical = dependencies
        .iter()
        .map(|requirement| {
            (
                requirement.module.to_string(),
                requirement.constraint.to_string(),
            )
        })
        .collect::<Vec<_>>();
    canonical.sort();
    canonical
}

pub(crate) fn canonical_manifest_dependencies(
    dependencies: &[ManifestDependency],
) -> Vec<(String, String)> {
    let mut canonical = dependencies
        .iter()
        .map(|requirement| {
            (
                requirement.module.to_string(),
                requirement.constraint.to_string(),
            )
        })
        .collect::<Vec<_>>();
    canonical.sort();
    canonical
}

fn parse_installed_package_manifest<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
    release: &ReleaseManifest,
) -> Result<PackageManifest, InstalledModuleError> {
    let path = module_dir.join("vo.package.json");
    let kind = super::source_integrity::entry_kind(
        fs,
        &path,
        locked,
        InstalledModuleField::PackageManifest,
    )?;
    super::source_integrity::require_regular_file(
        kind,
        &path,
        locked,
        InstalledModuleField::PackageManifest,
    )?;
    let bytes = fs
        .read_bytes_limited(&path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(|error| InstalledModuleError {
            module: locked.path.to_string(),
            version: locked.version.to_string(),
            field: InstalledModuleField::PackageManifest,
            kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
                detail: format!("cannot read cached vo.package.json: {error}"),
            }),
        })?;
    let found_size = u64::try_from(bytes.len()).unwrap_or(u64::MAX);
    let found_digest = Digest::from_sha256(&bytes);
    if found_size != release.package.size || found_digest != release.package.digest {
        return Err(InstalledModuleError {
            module: locked.path.to_string(),
            version: locked.version.to_string(),
            field: InstalledModuleField::PackageManifest,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: format!(
                    "{} ({} bytes)",
                    release.package.digest, release.package.size,
                ),
                found: format!("{} ({} bytes)", found_digest, found_size),
            }),
        });
    }
    PackageManifest::parse(&bytes).map_err(|error| InstalledModuleError {
        module: locked.path.to_string(),
        version: locked.version.to_string(),
        field: InstalledModuleField::PackageManifest,
        kind: Box::new(InstalledModuleErrorKind::ParseFailed {
            detail: error.to_string(),
        }),
    })
}

fn validate_cache_marker(
    locked: &LockedModule,
    field: InstalledModuleField,
    name: &str,
    actual: Option<&[u8]>,
    expected_value: &str,
) -> Result<(), InstalledModuleError> {
    let Some(actual) = actual else {
        return Err(InstalledModuleError {
            module: locked.path.to_string(),
            version: locked.version.to_string(),
            field,
            kind: Box::new(InstalledModuleErrorKind::Missing {
                detail: format!("cached module is missing {name}"),
            }),
        });
    };
    let expected = format!("{expected_value}\n");
    if actual == expected.as_bytes() {
        return Ok(());
    }
    let found = match std::str::from_utf8(actual) {
        Ok(text) => format!("{text:?}"),
        Err(_) => format!("<non-UTF-8 marker, {} bytes>", actual.len()),
    };
    Err(InstalledModuleError {
        module: locked.path.to_string(),
        version: locked.version.to_string(),
        field,
        kind: Box::new(InstalledModuleErrorKind::Mismatch {
            expected: format!("{expected:?}"),
            found,
        }),
    })
}

#[derive(Debug, Clone)]
pub(crate) struct InstalledModuleMetadata {
    pub release: ReleaseManifest,
    /// Exact authenticated `vo.release.json` byte length. Graph-wide
    /// materialization budgets use this value without rendering or reopening
    /// the manifest after validation.
    pub release_manifest_bytes: usize,
    pub extension: Option<crate::ext_manifest::ExtensionManifest>,
}

/// Validate that an installed module matches its `LockedModule` metadata.
///
/// `fs` is rooted at whatever directory contains the module cache.
/// `module_dir` is the path (relative to `fs` root) of the installed module
/// directory (e.g. `"github.com@acme@lib/1.2.3"`).
///
/// Release bytes are authenticated by `vo.lock`; package bytes are then
/// authenticated by the release. The package file table is compared against
/// the exact installed source tree before `vo.mod` is trusted.
pub fn validate_installed_module<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
) -> Result<(), InstalledModuleError> {
    validate_installed_module_with_metadata(fs, module_dir, locked).map(drop)
}

/// Validate an installed module and return extension metadata parsed from the
/// same `vo.mod` bytes that were checked against the lock entry.
pub(crate) fn validate_installed_module_with_metadata<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
) -> Result<InstalledModuleMetadata, InstalledModuleError> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();

    let (release, release_digest, release_manifest_bytes) =
        parse_installed_release_manifest(fs, module_dir, locked, module, &version)?;
    crate::lock::validate_locked_module_against_manifest(locked, &release, &release_digest)
        .map_err(|error| match error {
            crate::Error::LockedModuleMismatch {
                field,
                expected,
                found,
                ..
            } => InstalledModuleError {
                module: module.to_string(),
                version: version.clone(),
                field: InstalledModuleField::ReleaseManifest,
                kind: Box::new(InstalledModuleErrorKind::LockedModuleMismatch {
                    field,
                    expected,
                    found,
                }),
            },
            error => InstalledModuleError {
                module: module.to_string(),
                version: version.clone(),
                field: InstalledModuleField::ReleaseManifest,
                kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
                    detail: error.to_string(),
                }),
            },
        })?;
    let package = parse_installed_package_manifest(fs, module_dir, locked, &release)?;

    // Reconstruct the complete installed source set while validating tree
    // shape, entry kinds, portable paths, raw bytes and resource bounds.
    let installed_source = super::source_integrity::scan(fs, module_dir, locked, &package.files)?;
    validate_cache_marker(
        locked,
        InstalledModuleField::VersionMarker,
        crate::cache::layout::VERSION_MARKER,
        installed_source.version_marker_bytes.as_deref(),
        &locked.version.to_string(),
    )?;
    validate_cache_marker(
        locked,
        InstalledModuleField::SourceDigestMarker,
        crate::cache::layout::SOURCE_DIGEST_MARKER,
        installed_source.source_digest_marker_bytes.as_deref(),
        release.source.digest.as_str(),
    )?;
    if package.files != installed_source.entries {
        let detail = crate::schema::source_files::package_file_set_mismatch_detail(
            &package.files,
            &installed_source.entries,
        );
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::SourceFiles,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: "the exact authenticated vo.package.json file table".to_string(),
                found: detail,
            }),
        });
    }

    // Only bytes captured by the authenticated source scan may influence
    // runtime policy. Reopening this path would create a replacement window
    // between package verification and metadata parsing.
    let mod_file_path = module_dir.join("vo.mod");
    let mod_file_bytes = installed_source
        .mod_file_bytes
        .ok_or_else(|| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::Missing {
                detail: "cached module is missing vo.mod".to_string(),
            }),
        })?;
    let mod_content =
        std::str::from_utf8(&mod_file_bytes).map_err(|error| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::ParseFailed {
                detail: format!("vo.mod is not valid UTF-8: {error}"),
            }),
        })?;
    let mod_file =
        crate::schema::modfile::ModFile::parse(mod_content).map_err(|e| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::ParseFailed {
                detail: e.to_string(),
            }),
        })?;
    // Cached modules are fetched from the registry and therefore always
    // carry a canonical github identity; `local/*` could not be installed.
    if mod_file.module.as_github() != Some(&locked.path) {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: locked.path.to_string(),
                found: mod_file.module.to_string(),
            }),
        });
    }
    if mod_file.vo != release.vo {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: release.vo.to_string(),
                found: mod_file.vo.to_string(),
            }),
        });
    }
    let packaged_dependencies = canonical_mod_dependencies(&mod_file.dependencies);
    let published_dependencies = canonical_manifest_dependencies(&release.dependencies);
    if packaged_dependencies != published_dependencies {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: format!("published dependencies {published_dependencies:?}"),
                found: format!("packaged dependencies {packaged_dependencies:?}"),
            }),
        });
    }

    let mut ext_manifest = mod_file.extension;
    if let Some(extension) = ext_manifest.as_mut() {
        extension.manifest_path = match fs.root() {
            Some(root) => root.join(&mod_file_path),
            None => mod_file_path.clone(),
        };
    }
    crate::lock::validate_extension_manifest_against_release_manifest(
        ext_manifest.as_ref(),
        &release,
    )
    .map_err(|error| InstalledModuleError {
        module: module.to_string(),
        version: version.clone(),
        field: InstalledModuleField::ExtManifest,
        kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
            detail: error.to_string(),
        }),
    })?;

    Ok(InstalledModuleMetadata {
        release,
        release_manifest_bytes,
        extension: ext_manifest,
    })
}

/// Validate one installed artifact selected by its release identity.
///
/// `fs` is rooted at the module cache and `module_dir` must be the canonical
/// cache directory derived from `locked_module`. The artifact path and its
/// expected size/digest are derived from the release bytes authenticated by
/// the lock entry, so callers cannot redirect validation or substitute
/// unbound metadata.
pub fn validate_installed_artifact<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked_module: &LockedModule,
    artifact_id: &ArtifactId,
) -> Result<(), InstalledModuleError> {
    super::source_integrity::validate_module_directory_chain(
        fs,
        module_dir,
        locked_module,
        InstalledModuleField::Artifact,
    )?;

    let module = locked_module.path.as_str();
    let version = locked_module.version.to_string();
    let (release, release_digest, _) =
        parse_installed_release_manifest(fs, module_dir, locked_module, module, &version)?;
    crate::lock::validate_locked_module_against_manifest(locked_module, &release, &release_digest)
        .map_err(|error| artifact_validation_error(locked_module, error.to_string()))?;

    let mut matches = release
        .artifacts
        .iter()
        .filter(|artifact| artifact.id == *artifact_id);
    let Some(artifact) = matches.next() else {
        return Err(artifact_validation_error(
            locked_module,
            format!("artifact {artifact_id} is not declared by vo.release.json"),
        ));
    };
    if matches.next().is_some() {
        return Err(artifact_validation_error(
            locked_module,
            format!("artifact {artifact_id} is declared more than once by vo.release.json"),
        ));
    }
    let relative_artifact_path = crate::artifact::artifact_relative_path(artifact_id)
        .map_err(|detail| artifact_validation_error(locked_module, detail))?;
    validate_artifact_parent_chain(fs, module_dir, &relative_artifact_path, locked_module)?;
    let artifact_path = module_dir.join(relative_artifact_path);

    validate_installed_artifact_at_path(fs, &artifact_path, locked_module, artifact)
}

/// Validate one artifact entry obtained from an already authenticated
/// `InstalledModuleMetadata::release`.
///
/// Graph validators load and authenticate `vo.release.json` once per module,
/// then use this entry-level path for every declared artifact. This preserves
/// the exact release authority while avoiding a complete manifest reread and
/// reparse for each artifact in the same module.
pub(crate) fn validate_installed_artifact_from_metadata<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked_module: &LockedModule,
    artifact: &ManifestArtifact,
) -> Result<(), InstalledModuleError> {
    super::source_integrity::validate_module_directory_chain(
        fs,
        module_dir,
        locked_module,
        InstalledModuleField::Artifact,
    )?;
    let relative_artifact_path = crate::artifact::artifact_relative_path(&artifact.id)
        .map_err(|detail| artifact_validation_error(locked_module, detail))?;
    validate_artifact_parent_chain(fs, module_dir, &relative_artifact_path, locked_module)?;
    validate_installed_artifact_at_path(
        fs,
        &module_dir.join(relative_artifact_path),
        locked_module,
        artifact,
    )
}

fn validate_artifact_parent_chain<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    relative_artifact_path: &Path,
    locked_module: &LockedModule,
) -> Result<(), InstalledModuleError> {
    let mut directory = module_dir.to_path_buf();
    let parent = relative_artifact_path.parent().ok_or_else(|| {
        artifact_validation_error(
            locked_module,
            "artifact cache path has no parent directory".to_string(),
        )
    })?;
    let mut directories = Vec::new();
    for component in parent.components() {
        let std::path::Component::Normal(component) = component else {
            return Err(artifact_validation_error(
                locked_module,
                format!(
                    "artifact cache parent contains a non-canonical component: {}",
                    parent.display(),
                ),
            ));
        };
        directory.push(component);
        directories.push(directory.clone());
    }
    for directory in directories {
        super::source_integrity::require_exact_child_spelling(
            fs,
            &directory,
            locked_module,
            InstalledModuleField::Artifact,
        )?;
        let kind = super::source_integrity::entry_kind(
            fs,
            &directory,
            locked_module,
            InstalledModuleField::Artifact,
        )?;
        super::source_integrity::require_directory(
            kind,
            &directory,
            locked_module,
            InstalledModuleField::Artifact,
        )?;
    }
    Ok(())
}

fn artifact_validation_error(locked_module: &LockedModule, detail: String) -> InstalledModuleError {
    InstalledModuleError {
        module: locked_module.path.to_string(),
        version: locked_module.version.to_string(),
        field: InstalledModuleField::Artifact,
        kind: Box::new(InstalledModuleErrorKind::ValidationFailed { detail }),
    }
}

fn validate_installed_artifact_at_path<F: FileSystem>(
    fs: &F,
    artifact_path: &Path,
    locked_module: &LockedModule,
    artifact: &ManifestArtifact,
) -> Result<(), InstalledModuleError> {
    let module = locked_module.path.as_str();
    let version = locked_module.version.to_string();

    let kind = super::source_integrity::entry_kind(
        fs,
        artifact_path,
        locked_module,
        InstalledModuleField::Artifact,
    )?;
    super::source_integrity::require_regular_file(
        kind,
        artifact_path,
        locked_module,
        InstalledModuleField::Artifact,
    )?;

    if artifact.size == 0 || artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version,
            field: InstalledModuleField::Artifact,
            kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "declared artifact size {} must be within 1..={}",
                    artifact.size,
                    crate::MAX_MODULE_ARTIFACT_BYTES
                ),
            }),
        });
    }
    let expected_size = usize::try_from(artifact.size).map_err(|_| InstalledModuleError {
        module: module.to_string(),
        version: version.clone(),
        field: InstalledModuleField::Artifact,
        kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
            detail: format!(
                "declared artifact size {} is not addressable",
                artifact.size
            ),
        }),
    })?;

    let bytes = fs
        .read_bytes_limited(artifact_path, expected_size)
        .map_err(|error| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::Artifact,
            kind: if error.kind() == std::io::ErrorKind::NotFound {
                Box::new(InstalledModuleErrorKind::Missing {
                    detail: format!("artifact {} not in cache", artifact.id.name),
                })
            } else {
                Box::new(InstalledModuleErrorKind::ValidationFailed {
                    detail: format!("cannot read artifact {}: {error}", artifact.id.name),
                })
            },
        })?;

    if bytes.len() as u64 != artifact.size {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::Artifact,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: format!("{} bytes", artifact.size),
                found: format!("{} bytes", bytes.len()),
            }),
        });
    }

    let digest = Digest::from_sha256(&bytes);
    if digest != artifact.digest {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version,
            field: InstalledModuleField::Artifact,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: artifact.digest.as_str().to_string(),
                found: digest.as_str().to_string(),
            }),
        });
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use crate::identity::{ArtifactId, ModulePath};
    use crate::schema::lockfile::LockedDependency;
    use crate::schema::manifest::{
        ManifestArtifact, ManifestDependency, ManifestPackage, ManifestSource, ReleaseManifest,
    };
    use crate::schema::{PackageManifest, SourceFileEntry};
    use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
    use vo_common::vfs::MemoryFs;

    struct ReplacingModFileFs {
        inner: MemoryFs,
        mod_file_path: PathBuf,
        replacement: Vec<u8>,
        mod_file_reads: AtomicUsize,
    }

    impl FileSystem for ReplacingModFileFs {
        fn read_file(&self, path: &Path) -> std::io::Result<String> {
            self.inner.read_file(path)
        }

        fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            self.inner.read_bytes(path)
        }

        fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> std::io::Result<Vec<u8>> {
            if path == self.mod_file_path {
                let read = self.mod_file_reads.fetch_add(1, Ordering::SeqCst);
                if read > 0 {
                    if self.replacement.len() > max_bytes {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "replacement exceeds read limit",
                        ));
                    }
                    return Ok(self.replacement.clone());
                }
            }
            self.inner.read_bytes_limited(path, max_bytes)
        }

        fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
            self.inner.read_dir(path)
        }

        fn exists(&self, path: &Path) -> bool {
            self.inner.exists(path)
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.inner.is_dir(path)
        }

        fn entry_kind(&self, path: &Path) -> std::io::Result<vo_common::vfs::FileSystemEntryKind> {
            self.inner.entry_kind(path)
        }

        fn root(&self) -> Option<&Path> {
            self.inner.root()
        }
    }

    struct CachedModuleFixture {
        fs: MemoryFs,
        locked: LockedModule,
        module_dir: PathBuf,
        release_raw: String,
    }

    fn bindgen_ext_manifest() -> &'static str {
        concat!(
            "[extension]\n",
            "name = \"lib\"\n\n",
            "[extension.wasm]\n",
            "kind = \"bindgen\"\n",
            "wasm = \"lib.wasm\"\n",
            "js = \"lib.js\"\n",
        )
    }

    fn standalone_ext_manifest() -> &'static str {
        concat!(
            "[extension]\n",
            "name = \"lib\"\n\n",
            "[extension.wasm]\n",
            "kind = \"standalone\"\n",
            "wasm = \"lib.wasm\"\n",
        )
    }

    fn mod_content(ext_manifest: Option<&str>) -> String {
        let mut content = "module = \"github.com/acme/lib\"\nvo = \"^0.1.0\"\n".to_string();
        if let Some(ext_manifest) = ext_manifest {
            content.push('\n');
            content.push_str(ext_manifest);
        }
        content
    }

    fn wasm_artifact(kind: &str, name: &str, bytes: &[u8]) -> ManifestArtifact {
        ManifestArtifact {
            id: ArtifactId {
                kind: kind.to_string(),
                target: "wasm32-unknown-unknown".to_string(),
                name: name.to_string(),
            },
            size: bytes.len() as u64,
            digest: Digest::from_sha256(bytes),
        }
    }

    fn cached_module_fixture(
        ext_manifest: Option<&str>,
        artifacts: Vec<ManifestArtifact>,
    ) -> CachedModuleFixture {
        cached_module_fixture_with_contract(mod_content(ext_manifest), Vec::new(), artifacts)
    }

    fn cached_module_fixture_with_contract(
        mod_content: String,
        dependencies: Vec<ManifestDependency>,
        artifacts: Vec<ManifestArtifact>,
    ) -> CachedModuleFixture {
        cached_module_fixture_with_contract_and_markers(
            mod_content,
            dependencies,
            artifacts,
            true,
            true,
        )
    }

    fn cached_module_fixture_with_contract_and_markers(
        mod_content: String,
        dependencies: Vec<ManifestDependency>,
        mut artifacts: Vec<ManifestArtifact>,
        write_version_marker: bool,
        write_source_digest_marker: bool,
    ) -> CachedModuleFixture {
        let package_raw = PackageManifest {
            schema_version: 1,
            files: vec![SourceFileEntry {
                path: "vo.mod".to_string(),
                mode: crate::schema::SourceFileMode::Regular,
                size: mod_content.len() as u64,
                digest: Digest::from_sha256(mod_content.as_bytes()),
            }],
        }
        .render()
        .unwrap();
        artifacts.sort_by(|left, right| left.id.cmp(&right.id));
        let release = ReleaseManifest {
            schema_version: 2,
            module: ModulePath::parse("github.com/acme/lib").unwrap(),
            version: ExactVersion::parse("1.2.3").unwrap(),
            commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            dependencies,
            source: ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 3,
                digest: Digest::from_sha256(b"src"),
            },
            package: ManifestPackage {
                size: package_raw.len() as u64,
                digest: Digest::from_sha256(&package_raw),
            },
            artifacts,
        };
        let release_raw = release.render().unwrap();
        let locked = crate::lock::locked_module_from_manifest_raw(&release, release_raw.as_bytes());
        let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
        let mut fs = MemoryFs::new();
        fs.add_file(module_dir.join("vo.mod"), mod_content);
        fs.add_bytes(module_dir.join("vo.package.json"), package_raw);
        fs.add_file(module_dir.join("vo.release.json"), release_raw.clone());
        if write_version_marker {
            fs.add_file(
                module_dir.join(crate::cache::layout::VERSION_MARKER),
                format!("{}\n", locked.version),
            );
        }
        if write_source_digest_marker {
            fs.add_file(
                module_dir.join(crate::cache::layout::SOURCE_DIGEST_MARKER),
                format!("{}\n", release.source.digest),
            );
        }
        CachedModuleFixture {
            fs,
            locked,
            module_dir,
            release_raw,
        }
    }

    #[test]
    fn validate_installed_module_accepts_authenticated_v2_metadata() {
        let fixture = cached_module_fixture(None, Vec::new());

        validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked).unwrap();
    }

    #[test]
    fn validate_installed_module_requires_both_exact_cache_markers() {
        for (write_version, write_source_digest, field) in [
            (false, true, InstalledModuleField::VersionMarker),
            (true, false, InstalledModuleField::SourceDigestMarker),
        ] {
            let fixture = cached_module_fixture_with_contract_and_markers(
                mod_content(None),
                Vec::new(),
                Vec::new(),
                write_version,
                write_source_digest,
            );

            let error =
                validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
                    .unwrap_err();

            assert_eq!(error.field, field);
            assert!(matches!(
                error.kind.as_ref(),
                InstalledModuleErrorKind::Missing { .. }
            ));
        }
    }

    #[test]
    fn validate_installed_module_rejects_tampered_and_noncanonical_cache_markers() {
        for (name, field, bytes) in [
            (
                crate::cache::layout::VERSION_MARKER,
                InstalledModuleField::VersionMarker,
                b"1.2.4\n".to_vec(),
            ),
            (
                crate::cache::layout::SOURCE_DIGEST_MARKER,
                InstalledModuleField::SourceDigestMarker,
                format!("{}\n", Digest::from_sha256(b"tampered")).into_bytes(),
            ),
            (
                crate::cache::layout::VERSION_MARKER,
                InstalledModuleField::VersionMarker,
                b"1.2.3\n\n".to_vec(),
            ),
            (
                crate::cache::layout::SOURCE_DIGEST_MARKER,
                InstalledModuleField::SourceDigestMarker,
                format!("{}\n\n", Digest::from_sha256(b"src")).into_bytes(),
            ),
        ] {
            let mut fixture = cached_module_fixture(None, Vec::new());
            fixture.fs.add_bytes(fixture.module_dir.join(name), bytes);

            let error =
                validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
                    .unwrap_err();

            assert_eq!(error.field, field);
            assert!(matches!(
                error.kind.as_ref(),
                InstalledModuleErrorKind::Mismatch { .. }
            ));
        }
    }

    #[test]
    fn validate_installed_module_bounds_cache_markers_and_requires_regular_files() {
        for (name, field) in [
            (
                crate::cache::layout::VERSION_MARKER,
                InstalledModuleField::VersionMarker,
            ),
            (
                crate::cache::layout::SOURCE_DIGEST_MARKER,
                InstalledModuleField::SourceDigestMarker,
            ),
        ] {
            let mut oversized = cached_module_fixture(None, Vec::new());
            oversized.fs.add_bytes(
                oversized.module_dir.join(name),
                vec![b'x'; crate::cache::source_integrity::MAX_CACHE_MARKER_BYTES + 1],
            );

            let oversized_error =
                validate_installed_module(&oversized.fs, &oversized.module_dir, &oversized.locked)
                    .unwrap_err();

            assert_eq!(oversized_error.field, field);
            assert!(matches!(
                oversized_error.kind.as_ref(),
                InstalledModuleErrorKind::ValidationFailed { detail }
                    if detail.contains("512-byte limit")
            ));

            let mut wrong_type = cached_module_fixture_with_contract_and_markers(
                mod_content(None),
                Vec::new(),
                Vec::new(),
                name != crate::cache::layout::VERSION_MARKER,
                name != crate::cache::layout::SOURCE_DIGEST_MARKER,
            );
            wrong_type.fs.add_dir(wrong_type.module_dir.join(name));

            let wrong_type_error = validate_installed_module(
                &wrong_type.fs,
                &wrong_type.module_dir,
                &wrong_type.locked,
            )
            .unwrap_err();

            assert_eq!(wrong_type_error.field, field);
            assert!(matches!(
                wrong_type_error.kind.as_ref(),
                InstalledModuleErrorKind::ValidationFailed { detail }
                    if detail.contains("must be a regular file")
            ));
        }
    }

    #[test]
    fn validation_parses_the_vo_mod_bytes_captured_by_the_authenticated_scan() {
        let authenticated = concat!(
            "module = \"github.com/acme/lib\"\n",
            "vo = \"^0.1.0\"\n\n",
            "[extension]\n",
            "name = \"lib\"\n\n",
            "[extension.web]\n",
            "entry = \"authenticated\"\n",
            "capabilities = [\"trusted\"]\n",
        );
        let replacement = concat!(
            "module = \"github.com/acme/lib\"\n",
            "vo = \"^0.1.0\"\n\n",
            "[extension]\n",
            "name = \"lib\"\n\n",
            "[extension.web]\n",
            "entry = \"replacement\"\n",
            "capabilities = [\"untrusted\"]\n",
        );
        let fixture =
            cached_module_fixture_with_contract(authenticated.to_string(), Vec::new(), Vec::new());
        let fs = ReplacingModFileFs {
            mod_file_path: fixture.module_dir.join("vo.mod"),
            inner: fixture.fs,
            replacement: replacement.as_bytes().to_vec(),
            mod_file_reads: AtomicUsize::new(0),
        };

        let metadata =
            validate_installed_module_with_metadata(&fs, &fixture.module_dir, &fixture.locked)
                .unwrap();
        assert_eq!(metadata.release_manifest_bytes, fixture.release_raw.len());
        let web = metadata.extension.unwrap().web.unwrap();

        assert_eq!(web.entry.as_deref(), Some("authenticated"));
        assert_eq!(web.capabilities, vec![String::from("trusted")]);
        assert_eq!(fs.mod_file_reads.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn validate_installed_module_rejects_v3_lock_graph_drift() {
        let mut fixture = cached_module_fixture(None, Vec::new());
        fixture.locked.dependencies.push(LockedDependency {
            module: ModulePath::parse("github.com/acme/dep").unwrap(),
            constraint: DepConstraint::parse("^1.0.0").unwrap(),
        });

        let err = validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
            .unwrap_err();

        assert_eq!(err.field, InstalledModuleField::ReleaseManifest);
        assert!(matches!(
            err.kind.as_ref(),
            InstalledModuleErrorKind::LockedModuleMismatch { ref field, .. }
                if field == "dependencies"
        ));
    }

    #[test]
    fn validate_installed_module_rejects_release_and_package_tampering() {
        let mut fixture = cached_module_fixture(None, Vec::new());
        let tampered_release = fixture.release_raw.replace(
            "0123456789abcdef0123456789abcdef01234567",
            "1123456789abcdef0123456789abcdef01234567",
        );
        fixture
            .fs
            .add_file(fixture.module_dir.join("vo.release.json"), tampered_release);

        let release_error =
            validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
                .unwrap_err();
        assert_eq!(release_error.field, InstalledModuleField::ReleaseManifest);
        assert!(matches!(
            release_error.kind.as_ref(),
            InstalledModuleErrorKind::Mismatch { .. }
        ));

        let mut fixture = cached_module_fixture(None, Vec::new());
        fixture.fs.add_bytes(
            fixture.module_dir.join("vo.package.json"),
            br#"{"schema_version":1,"files":[]}"#.to_vec(),
        );
        let package_error =
            validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
                .unwrap_err();
        assert_eq!(package_error.field, InstalledModuleField::PackageManifest);
        assert!(matches!(
            package_error.kind.as_ref(),
            InstalledModuleErrorKind::Mismatch { .. }
        ));
    }

    #[test]
    fn validate_installed_module_rejects_packaged_ext_contract_mismatch() {
        let fixture = cached_module_fixture(
            Some(bindgen_ext_manifest()),
            vec![wasm_artifact("extension-wasm", "lib.wasm", b"wasm")],
        );

        let err = validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
            .unwrap_err();

        assert_eq!(err.field, InstalledModuleField::ExtManifest);
        assert!(matches!(
            err.kind.as_ref(),
            InstalledModuleErrorKind::ValidationFailed { ref detail }
                if detail.contains("missing declared artifacts")
        ));
    }

    #[test]
    fn validate_installed_module_rejects_published_artifacts_without_packaged_ext_manifest() {
        let fixture = cached_module_fixture(
            None,
            vec![wasm_artifact("extension-wasm", "lib.wasm", b"wasm")],
        );

        let err = validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
            .unwrap_err();

        assert_eq!(err.field, InstalledModuleField::ExtManifest);
        assert!(matches!(
            err.kind.as_ref(),
            InstalledModuleErrorKind::ValidationFailed { ref detail }
                if detail.contains("undeclared published artifacts")
        ));
    }

    #[test]
    fn validate_installed_module_rejects_packaged_requirement_drift() {
        let fixture = cached_module_fixture_with_contract(
            concat!(
                "module = \"github.com/acme/lib\"\n",
                "vo = \"^0.1.0\"\n\n",
                "[dependencies]\n",
                "\"github.com/acme/dep\" = \"^1.0.0\"\n",
            )
            .to_string(),
            Vec::new(),
            Vec::new(),
        );

        let error = validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
            .unwrap_err();

        assert_eq!(error.field, InstalledModuleField::ModFile);
        assert!(
            error.to_string().contains("packaged dependencies"),
            "{error}"
        );
    }

    #[test]
    fn validate_installed_artifact_derives_the_authenticated_release_cache_path() {
        let bytes = b"wasm";
        let artifact = wasm_artifact("extension-wasm", "lib.wasm", bytes);
        let artifact_id = artifact.id.clone();
        let mut fixture = cached_module_fixture(Some(standalone_ext_manifest()), vec![artifact]);
        fixture.fs.add_bytes(
            fixture
                .module_dir
                .join(crate::artifact::artifact_relative_path(&artifact_id).unwrap()),
            bytes.to_vec(),
        );

        validate_installed_artifact(
            &fixture.fs,
            &fixture.module_dir,
            &fixture.locked,
            &artifact_id,
        )
        .unwrap();

        let redirected = fixture.module_dir.join("elsewhere");
        let error =
            validate_installed_artifact(&fixture.fs, &redirected, &fixture.locked, &artifact_id)
                .unwrap_err();
        assert!(
            error.to_string().contains("module cache directory must be"),
            "{error}"
        );

        let metadata = validate_installed_module_with_metadata(
            &fixture.fs,
            &fixture.module_dir,
            &fixture.locked,
        )
        .unwrap();
        let authenticated_artifact = &metadata.release.artifacts[0];
        validate_installed_artifact_from_metadata(
            &fixture.fs,
            &fixture.module_dir,
            &fixture.locked,
            authenticated_artifact,
        )
        .unwrap();
        fixture.fs.add_bytes(
            fixture
                .module_dir
                .join(crate::artifact::artifact_relative_path(&artifact_id).unwrap()),
            b"tampered".to_vec(),
        );
        let error = validate_installed_artifact_from_metadata(
            &fixture.fs,
            &fixture.module_dir,
            &fixture.locked,
            authenticated_artifact,
        )
        .unwrap_err();
        assert_eq!(error.field, InstalledModuleField::Artifact);
    }

    #[test]
    fn validate_installed_artifact_rejects_invalid_or_ambiguous_release_identity() {
        let artifact = wasm_artifact("extension-wasm", "lib.wasm", b"wasm");
        let mut fixture =
            cached_module_fixture(Some(standalone_ext_manifest()), vec![artifact.clone()]);
        let invalid_raw = fixture
            .release_raw
            .replace("\"lib.wasm\"", "\"../outside.wasm\"");
        fixture.locked.release = Digest::from_sha256(invalid_raw.as_bytes());
        fixture
            .fs
            .add_file(fixture.module_dir.join("vo.release.json"), invalid_raw);
        let invalid_id = ArtifactId {
            kind: "extension-wasm".to_string(),
            target: "wasm32-unknown-unknown".to_string(),
            name: "../outside.wasm".to_string(),
        };

        let error = validate_installed_artifact(
            &fixture.fs,
            &fixture.module_dir,
            &fixture.locked,
            &invalid_id,
        )
        .unwrap_err();
        assert_eq!(error.field, InstalledModuleField::ReleaseManifest);
        assert!(error.to_string().contains("artifact name"), "{error}");

        let mut fixture =
            cached_module_fixture(Some(standalone_ext_manifest()), vec![artifact.clone()]);
        let mut release_value: serde_json::Value =
            serde_json::from_str(&fixture.release_raw).unwrap();
        let duplicate = release_value["artifacts"][0].clone();
        release_value["artifacts"]
            .as_array_mut()
            .unwrap()
            .push(duplicate);
        let duplicate_raw = serde_json::to_string_pretty(&release_value).unwrap();
        fixture.locked.release = Digest::from_sha256(duplicate_raw.as_bytes());
        fixture
            .fs
            .add_file(fixture.module_dir.join("vo.release.json"), duplicate_raw);

        let error = validate_installed_artifact(
            &fixture.fs,
            &fixture.module_dir,
            &fixture.locked,
            &artifact.id,
        )
        .unwrap_err();
        assert_eq!(error.field, InstalledModuleField::ReleaseManifest);
        assert!(
            error.to_string().contains("duplicates cache path"),
            "{error}"
        );
    }

    #[test]
    fn installed_source_tree_rejects_relocation_extra_binary_files_and_empty_directories() {
        let mut fixture = cached_module_fixture(None, Vec::new());
        let relocated = fixture.module_dir.with_file_name("1.2.4");
        for name in ["vo.mod", "vo.package.json", "vo.release.json"] {
            let bytes = fixture
                .fs
                .read_bytes(&fixture.module_dir.join(name))
                .unwrap();
            fixture.fs.add_bytes(relocated.join(name), bytes);
        }
        let error =
            validate_installed_module(&fixture.fs, &relocated, &fixture.locked).unwrap_err();
        assert_eq!(error.field, InstalledModuleField::Directory);
        assert!(error.to_string().contains("must be"), "{error}");

        let mut fixture = cached_module_fixture(None, Vec::new());
        fixture
            .fs
            .add_bytes(fixture.module_dir.join("unexpected.bin"), vec![0xff, 0xfe]);
        let error = validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
            .unwrap_err();
        assert_eq!(error.field, InstalledModuleField::SourceFiles);
        assert!(error.to_string().contains("first mismatch"), "{error}");

        let mut fixture = cached_module_fixture(None, Vec::new());
        fixture
            .fs
            .add_dir(fixture.module_dir.join("unexpected-empty-directory"));
        let error = validate_installed_module(&fixture.fs, &fixture.module_dir, &fixture.locked)
            .unwrap_err();
        assert_eq!(error.field, InstalledModuleField::SourceFiles);
        assert!(error.to_string().contains("empty directory"), "{error}");
    }

    #[cfg(unix)]
    #[test]
    fn installed_source_and_artifact_validation_reject_symlinked_cache_chains() {
        use std::os::unix::fs::symlink;
        use vo_common::vfs::RealFs;

        let artifact = wasm_artifact("extension-wasm", "lib.wasm", b"wasm");
        let artifact_id = artifact.id.clone();
        let fixture = cached_module_fixture(Some(standalone_ext_manifest()), vec![artifact]);
        let cache_key = crate::cache::layout::cache_key(&fixture.locked.path);

        for symlink_version in [false, true] {
            let root = tempfile::tempdir().unwrap();
            let outside = tempfile::tempdir().unwrap();
            let sentinel = outside.path().join("sentinel");
            std::fs::write(&sentinel, b"preserve").unwrap();
            if symlink_version {
                std::fs::create_dir(root.path().join(&cache_key)).unwrap();
                symlink(
                    outside.path(),
                    root.path()
                        .join(&cache_key)
                        .join(fixture.locked.version.to_string()),
                )
                .unwrap();
            } else {
                symlink(outside.path(), root.path().join(&cache_key)).unwrap();
            }
            let fs = RealFs::new(root.path());

            let source_error =
                validate_installed_module(&fs, &fixture.module_dir, &fixture.locked).unwrap_err();
            assert_eq!(source_error.field, InstalledModuleField::ReleaseManifest);

            let artifact_error = validate_installed_artifact(
                &fs,
                &fixture.module_dir,
                &fixture.locked,
                &artifact_id,
            )
            .unwrap_err();
            assert_eq!(artifact_error.field, InstalledModuleField::Artifact);
            assert!(
                artifact_error.to_string().contains("Symlink"),
                "{artifact_error}"
            );
            assert_eq!(std::fs::read(&sentinel).unwrap(), b"preserve");
        }
    }
}
