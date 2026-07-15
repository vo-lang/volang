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
use crate::schema::lockfile::{LockedArtifact, LockedModule};

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
    ModFile,
    VersionMarker,
    SourceDigest,
    SourceFiles,
    ReleaseManifest,
    WebManifest,
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
            InstalledModuleField::ModFile => "vo.mod",
            InstalledModuleField::VersionMarker => ".vo-version",
            InstalledModuleField::SourceDigest => ".vo-source-digest",
            InstalledModuleField::SourceFiles => "published source file set",
            InstalledModuleField::ReleaseManifest => "vo.release.json",
            InstalledModuleField::WebManifest => "vo.web.json",
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
) -> Result<(crate::schema::manifest::ReleaseManifest, Digest), InstalledModuleError> {
    let manifest_path = module_dir.join("vo.release.json");
    let manifest_bytes = fs
        .read_bytes_limited(&manifest_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(|_| InstalledModuleError {
            module: module.to_string(),
            version: version.to_string(),
            field: InstalledModuleField::ReleaseManifest,
            kind: Box::new(InstalledModuleErrorKind::Missing {
                detail: "cached module is missing vo.release.json".to_string(),
            }),
        })?;
    let manifest_digest = Digest::from_sha256(&manifest_bytes);
    if manifest_digest != locked.release_manifest {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.to_string(),
            field: InstalledModuleField::ReleaseManifest,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: locked.release_manifest.as_str().to_string(),
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
    Ok((manifest, manifest_digest))
}

pub(crate) fn canonical_mod_requirements(
    requirements: &[crate::schema::modfile::Require],
) -> Vec<(String, String)> {
    let mut canonical = requirements
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

pub(crate) fn canonical_manifest_requirements(
    requirements: &[crate::schema::manifest::ManifestRequire],
) -> Vec<(String, String)> {
    let mut canonical = requirements
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

fn validate_installed_web_manifest<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
    release: &crate::schema::manifest::ReleaseManifest,
    mod_file: &crate::schema::modfile::ModFile,
    installed_source_entries: &[crate::schema::SourceFileEntry],
) -> Result<Option<crate::schema::WebManifest>, InstalledModuleError> {
    let path = module_dir.join("vo.web.json");
    let kind =
        super::source_integrity::entry_kind(fs, &path, locked, InstalledModuleField::WebManifest)?;
    super::source_integrity::require_regular_file(
        kind,
        &path,
        locked,
        InstalledModuleField::WebManifest,
    )?;
    let bytes = fs
        .read_bytes_limited(&path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(|error| InstalledModuleError {
            module: locked.path.to_string(),
            version: locked.version.to_string(),
            field: InstalledModuleField::WebManifest,
            kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
                detail: format!("cannot read cached vo.web.json: {error}"),
            }),
        })?;
    let found_size = u64::try_from(bytes.len()).unwrap_or(u64::MAX);
    let found_digest = Digest::from_sha256(&bytes);
    if found_size != release.web_manifest.size || found_digest != release.web_manifest.digest {
        return Err(InstalledModuleError {
            module: locked.path.to_string(),
            version: locked.version.to_string(),
            field: InstalledModuleField::WebManifest,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: format!(
                    "{} ({} bytes)",
                    release.web_manifest.digest, release.web_manifest.size,
                ),
                found: format!("{} ({} bytes)", found_digest, found_size),
            }),
        });
    }
    let web = crate::schema::WebManifest::parse(&bytes).map_err(|error| InstalledModuleError {
        module: locked.path.to_string(),
        version: locked.version.to_string(),
        field: InstalledModuleField::WebManifest,
        kind: Box::new(InstalledModuleErrorKind::ParseFailed {
            detail: error.to_string(),
        }),
    })?;
    web.validate_release_contract(release)
        .and_then(|()| web.validate_mod_contract(mod_file))
        .map_err(|error| InstalledModuleError {
            module: locked.path.to_string(),
            version: locked.version.to_string(),
            field: InstalledModuleField::WebManifest,
            kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
                detail: error.to_string(),
            }),
        })?;
    if web.source != installed_source_entries {
        return Err(InstalledModuleError {
            module: locked.path.to_string(),
            version: locked.version.to_string(),
            field: InstalledModuleField::SourceFiles,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: format!("vo.web.json entries {:?}", web.source),
                found: format!("installed entries {installed_source_entries:?}"),
            }),
        });
    }
    Ok(Some(web))
}

/// Validate that an installed module matches its `LockedModule` metadata.
///
/// `fs` is rooted at whatever directory contains the module cache.
/// `module_dir` is the path (relative to `fs` root) of the installed module
/// directory (e.g. `"github.com@acme@lib/v1.2.3"`).
///
/// Checks (in order):
/// 1. `module_dir` exists and is a directory
/// 2. `vo.mod` exists, parses, module path matches, toolchain constraint matches
/// 3. `.vo-version` content matches `locked.version`
/// 4. `.vo-source-digest` content matches `locked.source`
/// 5. `vo.release.json` bytes SHA-256 matches `locked.release_manifest`
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
) -> Result<Option<crate::ext_manifest::ExtensionManifest>, InstalledModuleError> {
    let module = locked.path.as_str();
    let version = locked.version.to_string();

    // 1. Reconstruct the complete installed source set while validating the
    // tree shape, entry kinds, portable paths, and resource bounds.
    let installed_source_entries = super::source_integrity::scan(fs, module_dir, locked)?;

    // 2. vo.mod exists, parses, and fields match
    let mod_file_path = module_dir.join("vo.mod");
    let mod_content = fs
        .read_text_limited(&mod_file_path, vo_common::vfs::MAX_TEXT_FILE_BYTES)
        .map_err(|_| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::Missing {
                detail: "cached module is missing vo.mod".to_string(),
            }),
        })?;
    let mod_file =
        crate::schema::modfile::ModFile::parse(&mod_content).map_err(|e| InstalledModuleError {
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
    if mod_file.vo != locked.vo {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: locked.vo.to_string(),
                found: mod_file.vo.to_string(),
            }),
        });
    }

    // 3. .vo-version matches
    let version_marker_path = module_dir.join(super::layout::VERSION_MARKER);
    let installed_version_content =
        fs.read_text_limited(&version_marker_path, 1024)
            .map_err(|_| InstalledModuleError {
                module: module.to_string(),
                version: version.clone(),
                field: InstalledModuleField::VersionMarker,
                kind: Box::new(InstalledModuleErrorKind::Missing {
                    detail: "cached version metadata is missing".to_string(),
                }),
            })?;
    let installed_version = super::layout::canonical_marker_value(&installed_version_content)
        .ok_or_else(|| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::VersionMarker,
            kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
                detail: "marker must contain exactly one non-empty LF-terminated line".to_string(),
            }),
        })?;
    if installed_version != version {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::VersionMarker,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: version.clone(),
                found: installed_version.to_string(),
            }),
        });
    }

    // 4. .vo-source-digest matches
    let source_digest_path = module_dir.join(super::layout::SOURCE_DIGEST_MARKER);
    let installed_source_digest_content =
        fs.read_text_limited(&source_digest_path, 1024)
            .map_err(|_| InstalledModuleError {
                module: module.to_string(),
                version: version.clone(),
                field: InstalledModuleField::SourceDigest,
                kind: Box::new(InstalledModuleErrorKind::Missing {
                    detail: "cached source digest metadata is missing".to_string(),
                }),
            })?;
    let installed_source_digest = super::layout::canonical_marker_value(
        &installed_source_digest_content,
    )
    .ok_or_else(|| InstalledModuleError {
        module: module.to_string(),
        version: version.clone(),
        field: InstalledModuleField::SourceDigest,
        kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
            detail: "marker must contain exactly one non-empty LF-terminated line".to_string(),
        }),
    })?;
    if installed_source_digest != locked.source.as_str() {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::SourceDigest,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: locked.source.as_str().to_string(),
                found: installed_source_digest.to_string(),
            }),
        });
    }

    let (manifest, manifest_digest) =
        parse_installed_release_manifest(fs, module_dir, locked, module, &version)?;
    let packaged_requirements = canonical_mod_requirements(&mod_file.require);
    let published_requirements = canonical_manifest_requirements(&manifest.require);
    if packaged_requirements != published_requirements {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: format!("published requirements {published_requirements:?}"),
                found: format!("packaged requirements {packaged_requirements:?}"),
            }),
        });
    }
    crate::lock::validate_locked_module_against_manifest(locked, &manifest, &manifest_digest)
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

    let installed_source_set = crate::schema::canonical_source_file_set(&installed_source_entries)
        .map_err(|detail| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::SourceFiles,
            kind: Box::new(InstalledModuleErrorKind::ValidationFailed { detail }),
        })?;
    if installed_source_set.total_size != manifest.source.files_size
        || installed_source_set.digest != manifest.source.files_digest
    {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::SourceFiles,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: format!(
                    "{} ({} bytes)",
                    manifest.source.files_digest, manifest.source.files_size,
                ),
                found: format!(
                    "{} ({} bytes)",
                    installed_source_set.digest, installed_source_set.total_size,
                ),
            }),
        });
    }
    validate_installed_web_manifest(
        fs,
        module_dir,
        locked,
        &manifest,
        &mod_file,
        &installed_source_entries,
    )?;

    let mut ext_manifest = mod_file.extension;
    if let Some(extension) = ext_manifest.as_mut() {
        extension.manifest_path = match fs.root() {
            Some(root) => root.join(&mod_file_path),
            None => mod_file_path.clone(),
        };
    }
    crate::lock::validate_extension_manifest_against_release_manifest(
        ext_manifest.as_ref(),
        &manifest,
    )
    .map_err(|error| InstalledModuleError {
        module: module.to_string(),
        version,
        field: InstalledModuleField::ExtManifest,
        kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
            detail: error.to_string(),
        }),
    })?;

    Ok(ext_manifest)
}

/// Validate one installed artifact selected by its locked identity.
///
/// `fs` is rooted at the module cache and `module_dir` must be the canonical
/// cache directory derived from `locked_module`. The artifact path and its
/// expected size/digest are derived internally, so callers cannot redirect
/// validation to another file or substitute metadata that is absent from the
/// lock entry.
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

    let mut matches = locked_module
        .artifacts
        .iter()
        .filter(|artifact| artifact.id == *artifact_id);
    let Some(artifact) = matches.next() else {
        return Err(artifact_validation_error(
            locked_module,
            format!("artifact {artifact_id} is not pinned by vo.lock"),
        ));
    };
    if matches.next().is_some() {
        return Err(artifact_validation_error(
            locked_module,
            format!("artifact {artifact_id} is pinned more than once by vo.lock"),
        ));
    }
    let relative_artifact_path = crate::artifact::artifact_relative_path(artifact_id)
        .map_err(|detail| artifact_validation_error(locked_module, detail))?;
    validate_artifact_parent_chain(fs, module_dir, &relative_artifact_path, locked_module)?;
    let artifact_path = module_dir.join(relative_artifact_path);

    validate_installed_artifact_at_path(fs, &artifact_path, locked_module, artifact)
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
    artifact: &LockedArtifact,
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

    if artifact.size > crate::MAX_MODULE_ARTIFACT_BYTES {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version,
            field: InstalledModuleField::Artifact,
            kind: Box::new(InstalledModuleErrorKind::ValidationFailed {
                detail: format!(
                    "declared artifact size {} exceeds the {}-byte limit",
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

    use vo_common::vfs::MemoryFs;

    fn sample_manifest_with_wasm_only() -> &'static str {
        r#"{
  "schema_version": 1,
  "module": "github.com/acme/lib",
  "version": "v1.2.3",
  "commit": "0123456789abcdef0123456789abcdef01234567",
  "module_root": ".",
  "vo": "^0.1.0",
  "require": [],
  "source": {
    "name": "lib-v1.2.3-source.tar.gz",
    "size": 3,
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "files_size": 0,
    "files_digest": "sha256:4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945"
  },
  "web_manifest": {
    "size": 0,
    "digest": "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  },
  "artifacts": [
    {
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "lib.wasm",
      "size": 4,
      "digest": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    }
  ]
}"#
    }

    fn bindgen_ext_manifest() -> &'static str {
        concat!(
            "[extension]\n",
            "name = \"lib\"\n\n",
            "[extension.wasm]\n",
            "type = \"bindgen\"\n",
            "wasm = \"lib.wasm\"\n",
            "js_glue = \"lib.js\"\n",
        )
    }

    fn standalone_ext_manifest() -> &'static str {
        concat!(
            "[extension]\n",
            "name = \"lib\"\n\n",
            "[extension.wasm]\n",
            "type = \"standalone\"\n",
            "wasm = \"lib.wasm\"\n",
        )
    }

    fn cached_module_fs(
        manifest_raw: &str,
        ext_manifest: Option<&str>,
    ) -> (MemoryFs, LockedModule, PathBuf) {
        let mut manifest_value: serde_json::Value = serde_json::from_str(manifest_raw).unwrap();
        let module = manifest_value["module"].as_str().unwrap();
        let vo = manifest_value["vo"].as_str().unwrap();
        let mut mod_content = format!("module {module}\nvo {vo}\n");
        if let Some(ext_manifest) = ext_manifest {
            mod_content.push('\n');
            mod_content.push_str(ext_manifest);
        }
        let source_set =
            crate::schema::canonical_source_file_set(&[crate::schema::SourceFileEntry {
                path: "vo.mod".to_string(),
                size: mod_content.len() as u64,
                digest: Digest::from_sha256(mod_content.as_bytes()),
            }])
            .unwrap();
        manifest_value["source"]["files_size"] = source_set.total_size.into();
        manifest_value["source"]["files_digest"] = source_set.digest.to_string().into();
        let mod_file = crate::schema::modfile::ModFile::parse(&mod_content).unwrap();
        let web_artifacts = manifest_value["artifacts"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|artifact| artifact["kind"] != "extension-native")
            .map(|artifact| {
                let mut artifact = artifact.clone();
                artifact["path"] = artifact["name"].clone();
                artifact
            })
            .collect::<Vec<_>>();
        let web_extension = mod_file.extension.as_ref().map(|extension| {
            serde_json::json!({
                "name": extension.name,
                "include": extension.include,
                "wasm": extension.wasm,
                "web": extension.web,
            })
        });
        let web_raw = format!(
            "{}\n",
            serde_json::to_string_pretty(&serde_json::json!({
                "schema_version": 1,
                "module": manifest_value["module"],
                "version": manifest_value["version"],
                "commit": manifest_value["commit"],
                "module_root": manifest_value["module_root"],
                "vo": manifest_value["vo"],
                "require": manifest_value["require"],
                "source_digest": source_set.digest,
                "source": [{
                    "path": "vo.mod",
                    "size": mod_content.len() as u64,
                    "digest": Digest::from_sha256(mod_content.as_bytes()),
                }],
                "web": mod_file.web,
                "extension": web_extension,
                "artifacts": web_artifacts,
            }))
            .unwrap()
        );
        manifest_value["web_manifest"]["size"] = (web_raw.len() as u64).into();
        manifest_value["web_manifest"]["digest"] =
            Digest::from_sha256(web_raw.as_bytes()).to_string().into();
        let manifest_raw = format!(
            "{}\n",
            serde_json::to_string_pretty(&manifest_value).unwrap()
        );
        let manifest = crate::schema::manifest::ReleaseManifest::parse(&manifest_raw).unwrap();
        let locked =
            crate::lock::locked_module_from_manifest_raw(&manifest, manifest_raw.as_bytes());
        let module_dir = crate::cache::layout::relative_module_dir(&locked.path, &locked.version);
        let mut fs = MemoryFs::new();
        fs.add_file(module_dir.join("vo.mod"), mod_content);
        fs.add_file(module_dir.join("vo.web.json"), web_raw);
        fs.add_file(
            module_dir.join(super::super::layout::VERSION_MARKER),
            format!("{}\n", locked.version),
        );
        fs.add_file(
            module_dir.join(super::super::layout::SOURCE_DIGEST_MARKER),
            format!("{}\n", locked.source),
        );
        fs.add_file(module_dir.join("vo.release.json"), manifest_raw);
        (fs, locked, module_dir)
    }

    #[test]
    fn validate_installed_module_rejects_locked_artifact_set_mismatch() {
        let (fs, mut locked, module_dir) = cached_module_fs(sample_manifest_with_wasm_only(), None);
        locked.artifacts.clear();

        let err = validate_installed_module(&fs, &module_dir, &locked).unwrap_err();

        assert_eq!(err.field, InstalledModuleField::ReleaseManifest);
        assert!(matches!(
            err.kind.as_ref(),
            InstalledModuleErrorKind::LockedModuleMismatch { ref field, .. } if field == "artifacts"
        ));
    }

    #[test]
    fn validate_installed_module_rejects_packaged_ext_contract_mismatch() {
        let (fs, locked, module_dir) = cached_module_fs(
            sample_manifest_with_wasm_only(),
            Some(bindgen_ext_manifest()),
        );

        let err = validate_installed_module(&fs, &module_dir, &locked).unwrap_err();

        assert_eq!(err.field, InstalledModuleField::ExtManifest);
        assert!(matches!(
            err.kind.as_ref(),
            InstalledModuleErrorKind::ValidationFailed { ref detail }
                if detail.contains("missing declared artifacts")
        ));
    }

    #[test]
    fn validate_installed_module_rejects_published_artifacts_without_packaged_ext_manifest() {
        let (fs, locked, module_dir) = cached_module_fs(sample_manifest_with_wasm_only(), None);

        let err = validate_installed_module(&fs, &module_dir, &locked).unwrap_err();

        assert_eq!(err.field, InstalledModuleField::ExtManifest);
        assert!(matches!(
            err.kind.as_ref(),
            InstalledModuleErrorKind::ValidationFailed { ref detail }
                if detail.contains("undeclared published artifacts")
        ));
    }

    #[test]
    fn validate_installed_module_rejects_packaged_requirement_drift() {
        let (mut fs, locked, module_dir) = cached_module_fs(sample_manifest_with_wasm_only(), None);
        fs.add_file(
            module_dir.join("vo.mod"),
            concat!(
                "module github.com/acme/lib\n",
                "vo ^0.1.0\n",
                "require github.com/acme/dep ^1.0.0\n",
            ),
        );

        let error = validate_installed_module(&fs, &module_dir, &locked).unwrap_err();

        assert_eq!(error.field, InstalledModuleField::ModFile);
        assert!(error.to_string().contains("requirements"), "{error}");
    }

    #[test]
    fn validate_installed_module_rejects_non_canonical_marker_encodings() {
        for (marker, field) in [
            (
                super::super::layout::VERSION_MARKER,
                InstalledModuleField::VersionMarker,
            ),
            (
                super::super::layout::SOURCE_DIGEST_MARKER,
                InstalledModuleField::SourceDigest,
            ),
        ] {
            for content in ["", "v1.2.3", "v1.2.3\r\n", "v1.2.3\n\n", "\u{00a0}v1.2.3\n"] {
                let (mut fs, locked, module_dir) =
                    cached_module_fs(sample_manifest_with_wasm_only(), None);
                fs.add_file(module_dir.join(marker), content.to_string());

                let error = validate_installed_module(&fs, &module_dir, &locked).unwrap_err();

                assert_eq!(error.field, field, "{marker}: {content:?}: {error}");
            }
        }
    }

    #[test]
    fn validate_installed_artifact_derives_the_locked_cache_path() {
        let bytes = b"wasm";
        let manifest_raw = sample_manifest_with_wasm_only().replace(
            "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            Digest::from_sha256(bytes).as_str(),
        );
        let (mut fs, locked, module_dir) =
            cached_module_fs(&manifest_raw, Some(standalone_ext_manifest()));
        let artifact_id = locked.artifacts[0].id.clone();
        fs.add_bytes(
            module_dir.join(crate::artifact::artifact_relative_path(&artifact_id).unwrap()),
            bytes.to_vec(),
        );

        validate_installed_artifact(&fs, &module_dir, &locked, &artifact_id).unwrap();

        let redirected = module_dir.join("elsewhere");
        let error =
            validate_installed_artifact(&fs, &redirected, &locked, &artifact_id).unwrap_err();
        assert!(
            error.to_string().contains("module cache directory must be"),
            "{error}"
        );
    }

    #[test]
    fn validate_installed_artifact_rejects_invalid_or_ambiguous_locked_identity() {
        let (fs, mut locked, module_dir) = cached_module_fs(
            sample_manifest_with_wasm_only(),
            Some(standalone_ext_manifest()),
        );
        locked.artifacts[0].id.name = "../outside.wasm".to_string();
        let invalid_id = locked.artifacts[0].id.clone();

        let error =
            validate_installed_artifact(&fs, &module_dir, &locked, &invalid_id).unwrap_err();
        assert!(error.to_string().contains("artifact name"), "{error}");

        let (fs, mut locked, module_dir) = cached_module_fs(
            sample_manifest_with_wasm_only(),
            Some(standalone_ext_manifest()),
        );
        let duplicate = locked.artifacts[0].clone();
        let duplicate_id = duplicate.id.clone();
        locked.artifacts.push(duplicate);

        let error =
            validate_installed_artifact(&fs, &module_dir, &locked, &duplicate_id).unwrap_err();
        assert!(
            error.to_string().contains("pinned more than once"),
            "{error}"
        );
    }

    #[test]
    fn installed_source_tree_rejects_relocation_extra_binary_files_and_empty_directories() {
        let (fs, locked, module_dir) = cached_module_fs(sample_manifest_with_wasm_only(), None);
        let relocated = module_dir.with_file_name("v1.2.4");
        let error = validate_installed_module(&fs, &relocated, &locked).unwrap_err();
        assert_eq!(error.field, InstalledModuleField::Directory);
        assert!(error.to_string().contains("must be"), "{error}");

        let (mut fs, locked, module_dir) = cached_module_fs(sample_manifest_with_wasm_only(), None);
        fs.add_bytes(module_dir.join("unexpected.bin"), vec![0xff, 0xfe]);
        let error = validate_installed_module(&fs, &module_dir, &locked).unwrap_err();
        assert_eq!(error.field, InstalledModuleField::SourceFiles);
        assert!(error.to_string().contains("valid UTF-8"), "{error}");

        let (mut fs, locked, module_dir) = cached_module_fs(sample_manifest_with_wasm_only(), None);
        fs.add_dir(module_dir.join("unexpected-empty-directory"));
        let error = validate_installed_module(&fs, &module_dir, &locked).unwrap_err();
        assert_eq!(error.field, InstalledModuleField::SourceFiles);
        assert!(error.to_string().contains("empty directory"), "{error}");
    }

    #[cfg(unix)]
    #[test]
    fn installed_source_and_artifact_validation_reject_symlinked_cache_chains() {
        use std::os::unix::fs::symlink;
        use vo_common::vfs::RealFs;

        let (_, locked, module_dir) = cached_module_fs(
            sample_manifest_with_wasm_only(),
            Some(standalone_ext_manifest()),
        );
        let cache_key = crate::cache::layout::cache_key(&locked.path);
        let artifact_id = locked.artifacts[0].id.clone();

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
                        .join(locked.version.to_string()),
                )
                .unwrap();
            } else {
                symlink(outside.path(), root.path().join(&cache_key)).unwrap();
            }
            let fs = RealFs::new(root.path());

            let source_error = validate_installed_module(&fs, &module_dir, &locked).unwrap_err();
            assert_eq!(source_error.field, InstalledModuleField::Directory);
            assert!(
                source_error.to_string().contains("Symlink"),
                "{source_error}"
            );

            let artifact_error =
                validate_installed_artifact(&fs, &module_dir, &locked, &artifact_id).unwrap_err();
            assert_eq!(artifact_error.field, InstalledModuleField::Artifact);
            assert!(
                artifact_error.to_string().contains("Symlink"),
                "{artifact_error}"
            );
            assert_eq!(std::fs::read(&sentinel).unwrap(), b"preserve");
        }
    }
}
