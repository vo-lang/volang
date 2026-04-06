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
    pub kind: InstalledModuleErrorKind,
}

/// Which part of the installed module failed validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstalledModuleField {
    Directory,
    ModFile,
    VersionMarker,
    SourceDigest,
    ReleaseManifest,
    Artifact,
}

/// What went wrong.
#[derive(Debug)]
pub enum InstalledModuleErrorKind {
    Missing { detail: String },
    Mismatch { expected: String, found: String },
    ParseFailed { detail: String },
}

impl fmt::Display for InstalledModuleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let field = match self.field {
            InstalledModuleField::Directory => "directory",
            InstalledModuleField::ModFile => "vo.mod",
            InstalledModuleField::VersionMarker => ".vo-version",
            InstalledModuleField::SourceDigest => ".vo-source-digest",
            InstalledModuleField::ReleaseManifest => "vo.release.json",
            InstalledModuleField::Artifact => "artifact",
        };
        match &self.kind {
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
        }
    }
}

// ── Validation ───────────────────────────────────────────────────────────────

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
    let module = locked.path.as_str();
    let version = locked.version.to_string();

    // 1. Directory exists
    if !fs.exists(module_dir) || !fs.is_dir(module_dir) {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version,
            field: InstalledModuleField::Directory,
            kind: InstalledModuleErrorKind::Missing {
                detail: "source package not in cache".to_string(),
            },
        });
    }

    // 2. vo.mod exists, parses, and fields match
    let mod_file_path = module_dir.join("vo.mod");
    let mod_content = fs
        .read_file(&mod_file_path)
        .map_err(|_| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: InstalledModuleErrorKind::Missing {
                detail: "cached module is missing vo.mod".to_string(),
            },
        })?;
    let mod_file =
        crate::schema::modfile::ModFile::parse(&mod_content).map_err(|e| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: InstalledModuleErrorKind::ParseFailed {
                detail: e.to_string(),
            },
        })?;
    if mod_file.module != locked.path {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: InstalledModuleErrorKind::Mismatch {
                expected: locked.path.to_string(),
                found: mod_file.module.to_string(),
            },
        });
    }
    if mod_file.vo != locked.vo {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ModFile,
            kind: InstalledModuleErrorKind::Mismatch {
                expected: locked.vo.to_string(),
                found: mod_file.vo.to_string(),
            },
        });
    }

    // 3. .vo-version matches
    let version_marker_path = module_dir.join(super::layout::VERSION_MARKER);
    let installed_version = fs
        .read_file(&version_marker_path)
        .ok()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .ok_or_else(|| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::VersionMarker,
            kind: InstalledModuleErrorKind::Missing {
                detail: "cached version metadata is missing".to_string(),
            },
        })?;
    if installed_version != version {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::VersionMarker,
            kind: InstalledModuleErrorKind::Mismatch {
                expected: version.clone(),
                found: installed_version,
            },
        });
    }

    // 4. .vo-source-digest matches
    let source_digest_path = module_dir.join(super::layout::SOURCE_DIGEST_MARKER);
    let installed_source_digest = fs
        .read_file(&source_digest_path)
        .ok()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .ok_or_else(|| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::SourceDigest,
            kind: InstalledModuleErrorKind::Missing {
                detail: "cached source digest metadata is missing".to_string(),
            },
        })?;
    if installed_source_digest != locked.source.as_str() {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::SourceDigest,
            kind: InstalledModuleErrorKind::Mismatch {
                expected: locked.source.as_str().to_string(),
                found: installed_source_digest,
            },
        });
    }

    // 5. vo.release.json manifest digest matches
    let manifest_path = module_dir.join("vo.release.json");
    let manifest_bytes = fs
        .read_bytes(&manifest_path)
        .map_err(|_| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::ReleaseManifest,
            kind: InstalledModuleErrorKind::Missing {
                detail: "cached module is missing vo.release.json".to_string(),
            },
        })?;
    let manifest_digest = Digest::from_sha256(&manifest_bytes);
    if manifest_digest != locked.release_manifest {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version,
            field: InstalledModuleField::ReleaseManifest,
            kind: InstalledModuleErrorKind::Mismatch {
                expected: locked.release_manifest.as_str().to_string(),
                found: manifest_digest.as_str().to_string(),
            },
        });
    }

    Ok(())
}

/// Validate that an installed artifact matches its `LockedArtifact` metadata.
///
/// `fs` is rooted at whatever directory contains the module cache.
/// `artifact_path` is the path (relative to `fs` root) of the artifact file.
pub fn validate_installed_artifact<F: FileSystem>(
    fs: &F,
    artifact_path: &Path,
    locked_module: &LockedModule,
    artifact: &LockedArtifact,
) -> Result<(), InstalledModuleError> {
    let module = locked_module.path.as_str();
    let version = locked_module.version.to_string();

    let bytes = fs
        .read_bytes(artifact_path)
        .map_err(|_| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::Artifact,
            kind: InstalledModuleErrorKind::Missing {
                detail: format!("artifact {} not in cache", artifact.id.name),
            },
        })?;

    if bytes.len() as u64 != artifact.size {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::Artifact,
            kind: InstalledModuleErrorKind::Mismatch {
                expected: format!("{} bytes", artifact.size),
                found: format!("{} bytes", bytes.len()),
            },
        });
    }

    let digest = Digest::from_sha256(&bytes);
    if digest != artifact.digest {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version,
            field: InstalledModuleField::Artifact,
            kind: InstalledModuleErrorKind::Mismatch {
                expected: artifact.digest.as_str().to_string(),
                found: digest.as_str().to_string(),
            },
        });
    }

    Ok(())
}
