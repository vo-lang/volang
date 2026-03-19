use std::path::{Path, PathBuf};

use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::version::ExactVersion;
use crate::registry::Registry;
use crate::schema::lockfile::{LockFile, LockedArtifact, LockedModule};
use crate::Error;

/// Cache directory layout helper.
/// Cache key: `<cache_root>/<module_path_encoded>/<version>/`
///
/// The module path is encoded by replacing `/` with `@` (which is forbidden
/// in module-path segments) so that different paths never collide.
/// For example `github.com/acme/lib` → `github.com@acme@lib`.
pub fn cache_dir(cache_root: &Path, module: &ModulePath, version: &ExactVersion) -> PathBuf {
    cache_root
        .join(module.as_str().replace('/', "@"))
        .join(version.to_string())
}

/// Relative cache directory for a module (no cache_root prefix).
/// Useful as a VFS module root key.
pub fn cache_relative_dir(module: &ModulePath, version: &ExactVersion) -> PathBuf {
    PathBuf::from(module.as_str().replace('/', "@"))
        .join(version.to_string())
}

/// Check if a source package is already cached and valid.
pub fn is_source_cached(cache_root: &Path, locked: &LockedModule) -> bool {
    let dir = cache_dir(cache_root, &locked.path, &locked.version);
    let marker = dir.join(".vo-source-digest");
    marker.exists()
}

/// Check if a specific artifact is cached and valid.
pub fn is_artifact_cached(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
) -> bool {
    let dir = cache_dir(cache_root, &locked.path, &locked.version);
    let art_path = dir.join("artifacts").join(&artifact.id.name);
    art_path.exists()
}

/// Download and verify a source package into the cache.
pub fn download_source(
    cache_root: &Path,
    locked: &LockedModule,
    registry: &dyn Registry,
    source_asset_name: &str,
) -> Result<(), Error> {
    let data = registry.fetch_source_package(&locked.path, &locked.version, source_asset_name)?;

    // Verify digest
    let computed = Digest::from_sha256(&data);
    if computed != locked.source {
        return Err(Error::DigestMismatch {
            context: format!("source package for {} {}", locked.path, locked.version),
            expected: locked.source.as_str().to_string(),
            found: computed.as_str().to_string(),
        });
    }

    // Extract to cache directory
    let dir = cache_dir(cache_root, &locked.path, &locked.version);
    std::fs::create_dir_all(&dir)?;

    let gz = flate2::read::GzDecoder::new(data.as_slice());
    let mut archive = tar::Archive::new(gz);
    archive.unpack(&dir)?;

    // Write metadata files used by compile-time frozen-build validation
    std::fs::write(dir.join(".vo-source-digest"), format!("{}\n", locked.source))?;
    std::fs::write(dir.join(".vo-version"), format!("{}\n", locked.version))?;

    Ok(())
}

/// Download and verify a target-specific artifact into the cache.
pub fn download_artifact(
    cache_root: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
    registry: &dyn Registry,
) -> Result<(), Error> {
    let data = registry.fetch_artifact(&locked.path, &locked.version, &artifact.id.name)?;

    // Verify size
    if data.len() as u64 != artifact.size {
        return Err(Error::DigestMismatch {
            context: format!(
                "artifact {} for {} {}: size mismatch",
                artifact.id.name, locked.path, locked.version
            ),
            expected: format!("{} bytes", artifact.size),
            found: format!("{} bytes", data.len()),
        });
    }

    // Verify digest
    let computed = Digest::from_sha256(&data);
    if computed != artifact.digest {
        return Err(Error::DigestMismatch {
            context: format!(
                "artifact {} for {} {}",
                artifact.id.name, locked.path, locked.version
            ),
            expected: artifact.digest.as_str().to_string(),
            found: computed.as_str().to_string(),
        });
    }

    let dir = cache_dir(cache_root, &locked.path, &locked.version);
    let art_dir = dir.join("artifacts");
    std::fs::create_dir_all(&art_dir)?;
    std::fs::write(art_dir.join(&artifact.id.name), &data)?;

    Ok(())
}

/// Verify that all locked modules and their required artifacts are present in the cache
/// with correct digests. This is the frozen-build verification step.
///
/// Returns an error describing the first missing or invalid entry.
pub fn verify_frozen_cache(cache_root: &Path, lock_file: &LockFile) -> Result<(), Error> {
    for locked in &lock_file.resolved {
        if !is_source_cached(cache_root, locked) {
            return Err(Error::MissingArtifact {
                module: locked.path.as_str().to_string(),
                version: locked.version.to_string(),
                detail: "source package not in cache".to_string(),
            });
        }

        // Verify the cached source marker matches the locked digest
        let dir = cache_dir(cache_root, &locked.path, &locked.version);
        let marker = dir.join(".vo-source-digest");
        if let Ok(contents) = std::fs::read_to_string(&marker) {
            if contents.trim() != locked.source.as_str() {
                // Cache entry is stale — treat as missing per spec §8.3
                return Err(Error::MissingArtifact {
                    module: locked.path.as_str().to_string(),
                    version: locked.version.to_string(),
                    detail: "cached source digest does not match vo.lock".to_string(),
                });
            }
        }

        for artifact in &locked.artifacts {
            if !is_artifact_cached(cache_root, locked, artifact) {
                return Err(Error::MissingArtifact {
                    module: locked.path.as_str().to_string(),
                    version: locked.version.to_string(),
                    detail: format!("artifact {} not in cache", artifact.id.name),
                });
            }
        }
    }
    Ok(())
}

/// Download all missing source packages and artifacts for the locked graph.
pub fn download_all(
    cache_root: &Path,
    lock_file: &LockFile,
    registry: &dyn Registry,
) -> Result<(), Error> {
    for locked in &lock_file.resolved {
        if !is_source_cached(cache_root, locked) {
            // Fetch manifest raw bytes — used to get asset name and to write
            // `vo.release.json` into the cache for frozen-build validation.
            let (manifest, manifest_raw) =
                registry.fetch_manifest_raw(&locked.path, &locked.version)?;

            // Verify the fetched manifest digest matches the locked value.
            let computed = Digest::from_sha256(&manifest_raw);
            if computed != locked.release_manifest {
                return Err(Error::DigestMismatch {
                    context: format!(
                        "release manifest for {} {}",
                        locked.path, locked.version
                    ),
                    expected: locked.release_manifest.as_str().to_string(),
                    found: computed.as_str().to_string(),
                });
            }

            download_source(cache_root, locked, registry, &manifest.source.name)?;
            write_release_manifest(cache_root, locked, &manifest_raw)?;
        }
        for artifact in &locked.artifacts {
            if !is_artifact_cached(cache_root, locked, artifact) {
                download_artifact(cache_root, locked, artifact, registry)?;
            }
        }
    }
    Ok(())
}

/// Write the release manifest (`vo.release.json`) into the module cache directory.
fn write_release_manifest(
    cache_root: &Path,
    locked: &LockedModule,
    manifest_raw: &[u8],
) -> Result<(), Error> {
    let dir = cache_dir(cache_root, &locked.path, &locked.version);
    std::fs::write(dir.join("vo.release.json"), manifest_raw)?;
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_dir() {
        let root = Path::new("/tmp/vo-cache");
        let mp = ModulePath::parse("github.com/acme/lib").unwrap();
        let v = ExactVersion::parse("v1.2.3").unwrap();
        let dir = cache_dir(root, &mp, &v);
        assert_eq!(
            dir,
            PathBuf::from("/tmp/vo-cache/github.com@acme@lib/v1.2.3")
        );
    }

    #[test]
    fn test_digest_from_sha256() {
        let d = Digest::from_sha256(b"hello");
        assert_eq!(
            d.as_str(),
            "sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
        );
    }
}
