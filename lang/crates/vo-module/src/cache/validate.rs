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
    pub kind: Box<InstalledModuleErrorKind>,
}

/// Which part of the installed module failed validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstalledModuleField {
    Directory,
    ModFile,
    VersionMarker,
    SourceDigest,
    ReleaseManifest,
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
            InstalledModuleField::ReleaseManifest => "vo.release.json",
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
        .read_bytes(&manifest_path)
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

fn read_installed_extension_manifest<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    module: &str,
    version: &str,
) -> Result<Option<crate::ext_manifest::ExtensionManifest>, InstalledModuleError> {
    let mod_path = module_dir.join("vo.mod");
    if !fs.exists(&mod_path) {
        return Ok(None);
    }
    let content = fs.read_file(&mod_path).map_err(|_| InstalledModuleError {
        module: module.to_string(),
        version: version.to_string(),
        field: InstalledModuleField::ExtManifest,
        kind: Box::new(InstalledModuleErrorKind::Missing {
            detail: "cached module is missing readable vo.mod".to_string(),
        }),
    })?;
    crate::schema::modfile::ModFile::parse(&content)
        .map(|mod_file| mod_file.extension)
        .map_err(|error| InstalledModuleError {
            module: module.to_string(),
            version: version.to_string(),
            field: InstalledModuleField::ExtManifest,
            kind: Box::new(InstalledModuleErrorKind::ParseFailed {
                detail: error.to_string(),
            }),
        })
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
    let module = locked.path.as_str();
    let version = locked.version.to_string();

    // 1. Directory exists
    if !fs.exists(module_dir) || !fs.is_dir(module_dir) {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version,
            field: InstalledModuleField::Directory,
            kind: Box::new(InstalledModuleErrorKind::Missing {
                detail: "source package not in cache".to_string(),
            }),
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
    let installed_version = fs
        .read_file(&version_marker_path)
        .ok()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .ok_or_else(|| InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::VersionMarker,
            kind: Box::new(InstalledModuleErrorKind::Missing {
                detail: "cached version metadata is missing".to_string(),
            }),
        })?;
    if installed_version != version {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::VersionMarker,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: version.clone(),
                found: installed_version,
            }),
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
            kind: Box::new(InstalledModuleErrorKind::Missing {
                detail: "cached source digest metadata is missing".to_string(),
            }),
        })?;
    if installed_source_digest != locked.source.as_str() {
        return Err(InstalledModuleError {
            module: module.to_string(),
            version: version.clone(),
            field: InstalledModuleField::SourceDigest,
            kind: Box::new(InstalledModuleErrorKind::Mismatch {
                expected: locked.source.as_str().to_string(),
                found: installed_source_digest,
            }),
        });
    }

    let (manifest, manifest_digest) =
        parse_installed_release_manifest(fs, module_dir, locked, module, &version)?;
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

    let ext_manifest = read_installed_extension_manifest(fs, module_dir, module, &version)?;
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
            kind: Box::new(InstalledModuleErrorKind::Missing {
                detail: format!("artifact {} not in cache", artifact.id.name),
            }),
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
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
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

    fn cached_module_fs(
        manifest_raw: &str,
        ext_manifest: Option<&str>,
    ) -> (MemoryFs, LockedModule, PathBuf) {
        let manifest = crate::schema::manifest::ReleaseManifest::parse(manifest_raw).unwrap();
        let locked =
            crate::lock::locked_module_from_manifest_raw(&manifest, manifest_raw.as_bytes());
        let module_dir =
            crate::cache::layout::relative_module_dir(locked.path.as_str(), &locked.version);
        let mut fs = MemoryFs::new();
        let mut mod_content = format!("module {}\nvo {}\n", locked.path, locked.vo);
        if let Some(ext_manifest) = ext_manifest {
            mod_content.push('\n');
            mod_content.push_str(ext_manifest);
        }
        fs.add_file(module_dir.join("vo.mod"), mod_content);
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
}
