use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::schema::manifest::ReleaseManifest;
use crate::schema::lockfile::{LockFile, LockRoot, LockedArtifact, LockedModule};
use crate::schema::modfile::ModFile;
use crate::solver::ResolvedGraph;
use crate::Error;

fn manifest_deps(manifest: &ReleaseManifest) -> Vec<ModulePath> {
    let mut deps: Vec<ModulePath> = manifest.require.iter().map(|r| r.module.clone()).collect();
    deps.sort();
    deps
}

fn manifest_artifacts(manifest: &ReleaseManifest) -> Vec<LockedArtifact> {
    let mut artifacts: Vec<LockedArtifact> = manifest
        .artifacts
        .iter()
        .map(|artifact| LockedArtifact {
            id: artifact.id.clone(),
            size: artifact.size,
            digest: artifact.digest.clone(),
        })
        .collect();
    artifacts.sort_by(|a, b| a.id.cmp(&b.id));
    artifacts
}

fn format_deps(deps: &[ModulePath]) -> String {
    deps.iter()
        .map(|dep| dep.as_str().to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_artifacts(artifacts: &[LockedArtifact]) -> String {
    artifacts
        .iter()
        .map(|artifact| {
            format!(
                "{} {} {} {} {}",
                artifact.id.kind,
                artifact.id.target,
                artifact.id.name,
                artifact.size,
                artifact.digest,
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn locked_module_from_release_manifest(
    manifest: &ReleaseManifest,
    manifest_digest: Digest,
) -> LockedModule {
    LockedModule {
        path: manifest.module.clone(),
        version: manifest.version.clone(),
        vo: manifest.vo.clone(),
        commit: manifest.commit.clone(),
        release_manifest: manifest_digest,
        source: manifest.source.digest.clone(),
        deps: manifest_deps(manifest),
        artifacts: manifest_artifacts(manifest),
    }
}

pub fn locked_module_from_manifest_raw(
    manifest: &ReleaseManifest,
    manifest_raw: &[u8],
) -> LockedModule {
    locked_module_from_release_manifest(manifest, Digest::from_sha256(manifest_raw))
}

pub fn validate_locked_module_against_manifest(
    locked: &LockedModule,
    manifest: &ReleaseManifest,
    manifest_digest: &Digest,
) -> Result<(), Error> {
    let expected = locked_module_from_release_manifest(manifest, manifest_digest.clone());
    if locked.path != expected.path {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "path".to_string(),
            expected: expected.path.to_string(),
            found: locked.path.to_string(),
        });
    }
    if locked.version != expected.version {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "version".to_string(),
            expected: expected.version.to_string(),
            found: locked.version.to_string(),
        });
    }
    if locked.vo != expected.vo {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "vo".to_string(),
            expected: expected.vo.to_string(),
            found: locked.vo.to_string(),
        });
    }
    if locked.commit != expected.commit {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "commit".to_string(),
            expected: expected.commit,
            found: locked.commit.clone(),
        });
    }
    if locked.release_manifest != expected.release_manifest {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "release_manifest".to_string(),
            expected: expected.release_manifest.to_string(),
            found: locked.release_manifest.to_string(),
        });
    }
    if locked.source != expected.source {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "source".to_string(),
            expected: expected.source.to_string(),
            found: locked.source.to_string(),
        });
    }
    if locked.deps != expected.deps {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "deps".to_string(),
            expected: format_deps(&expected.deps),
            found: format_deps(&locked.deps),
        });
    }
    if locked.artifacts.len() != expected.artifacts.len() {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "artifacts".to_string(),
            expected: format_artifacts(&expected.artifacts),
            found: format_artifacts(&locked.artifacts),
        });
    }
    for (found, expected_artifact) in locked.artifacts.iter().zip(expected.artifacts.iter()) {
        if found.id != expected_artifact.id
            || found.size != expected_artifact.size
            || found.digest != expected_artifact.digest
        {
            return Err(Error::LockedModuleMismatch {
                module: locked.path.to_string(),
                field: format!(
                    "artifact {} {} {}",
                    found.id.kind, found.id.target, found.id.name,
                ),
                expected: format_artifacts(std::slice::from_ref(expected_artifact)),
                found: format_artifacts(std::slice::from_ref(found)),
            });
        }
    }
    Ok(())
}

/// Generate a `vo.lock` from a resolved graph and the root module file.
pub fn generate_lock(
    root_mod: &ModFile,
    graph: &ResolvedGraph,
    created_by: &str,
) -> Result<LockFile, Error> {
    let root = LockRoot {
        module: root_mod.module.clone(),
        vo: root_mod.vo.clone(),
    };

    let mut resolved: Vec<LockedModule> = Vec::new();
    for (mp, rm) in &graph.modules {
        let locked = locked_module_from_manifest_raw(&rm.manifest, &rm.manifest_raw);
        debug_assert_eq!(locked.path, *mp);
        debug_assert_eq!(locked.version, rm.version);
        resolved.push(locked);
    }

    resolved.sort_by(|a, b| a.path.cmp(&b.path));

    Ok(LockFile {
        version: 1,
        created_by: created_by.to_string(),
        root,
        resolved,
    })
}

/// Verify that root `vo.mod` and root `vo.lock` are consistent.
/// Per spec §12.1: root.module and root.vo MUST exactly match.
pub fn verify_root_consistency(mod_file: &ModFile, lock_file: &LockFile) -> Result<(), Error> {
    if mod_file.module != lock_file.root.module {
        return Err(Error::RootMismatch {
            field: "module".to_string(),
            mod_value: mod_file.module.to_string(),
            lock_value: lock_file.root.module.to_string(),
        });
    }
    if mod_file.vo != lock_file.root.vo {
        return Err(Error::RootMismatch {
            field: "vo".to_string(),
            mod_value: mod_file.vo.to_string(),
            lock_value: lock_file.root.vo.to_string(),
        });
    }
    Ok(())
}

/// Verify that all direct requirements in `vo.mod` are present in `vo.lock`,
/// and that no orphaned modules remain.
pub fn verify_graph_completeness(mod_file: &ModFile, lock_file: &LockFile) -> Result<(), Error> {
    // Every direct requirement must have a resolved entry
    for req in &mod_file.require {
        if lock_file.find(&req.module).is_none() {
            return Err(Error::LockFileParse(format!(
                "vo.mod requires {} but it is not in vo.lock",
                req.module
            )));
        }
    }

    // Every resolved module must be reachable from root's direct requirements,
    // and every dep listed in a locked module must itself be present in the lock.
    let mut reachable = std::collections::HashSet::new();
    let mut stack: Vec<&ModulePath> = mod_file.require.iter().map(|r| &r.module).collect();
    while let Some(mp) = stack.pop() {
        if !reachable.insert(mp.as_str()) {
            continue;
        }
        let lm = lock_file.find(mp).ok_or_else(|| {
            Error::LockFileParse(format!(
                "transitive dependency {} is referenced but not present in vo.lock",
                mp
            ))
        })?;
        for dep in &lm.deps {
            stack.push(dep);
        }
    }
    for lm in &lock_file.resolved {
        if !reachable.contains(lm.path.as_str()) {
            return Err(Error::LockFileParse(format!(
                "vo.lock contains orphaned module: {}",
                lm.path
            )));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_manifest() -> ReleaseManifest {
        ReleaseManifest::parse(
            r#"{
  "schema_version": 1,
  "module": "github.com/acme/lib",
  "version": "v1.2.3",
  "commit": "0123456789abcdef0123456789abcdef01234567",
  "module_root": ".",
  "vo": "^0.1.0",
  "require": [
    { "module": "github.com/acme/util", "constraint": "^0.1.0" }
  ],
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
}"#,
        )
        .unwrap()
    }

    #[test]
    fn test_verify_root_consistency_ok() {
        let mf = ModFile::parse("module github.com/acme/app\nvo ^1.0.0\n").unwrap();
        let lf_content = r#"version = 1
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/app"
vo = "^1.0.0"
"#;
        let lf = LockFile::parse(lf_content).unwrap();
        assert!(verify_root_consistency(&mf, &lf).is_ok());
    }

    #[test]
    fn test_verify_root_consistency_mismatch() {
        let mf = ModFile::parse("module github.com/acme/app\nvo ^1.0.0\n").unwrap();
        let lf_content = r#"version = 1
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/other"
vo = "^1.0.0"
"#;
        let lf = LockFile::parse(lf_content).unwrap();
        assert!(verify_root_consistency(&mf, &lf).is_err());
    }

    #[test]
    fn test_verify_graph_completeness_missing_transitive_dep() {
        let mf = ModFile::parse(
            "module github.com/acme/app\nvo ^1.0.0\nrequire github.com/acme/lib ^1.0.0\n",
        )
        .unwrap();
        // lib declares a dep on util, but util is NOT in the lock file.
        let lf_content = r#"version = 1
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[resolved]]
path = "github.com/acme/lib"
version = "v1.0.0"
vo = "^1.0.0"
commit = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
deps = ["github.com/acme/util"]
"#;
        let lf = LockFile::parse(lf_content).unwrap();
        let result = verify_graph_completeness(&mf, &lf);
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("github.com/acme/util"),
            "expected error about missing util, got: {msg}"
        );
    }

    #[test]
    fn test_verify_graph_completeness_orphaned_module() {
        let mf = ModFile::parse("module github.com/acme/app\nvo ^1.0.0\n").unwrap();
        // Lock has a resolved module that nobody depends on.
        let lf_content = r#"version = 1
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[resolved]]
path = "github.com/acme/orphan"
version = "v1.0.0"
vo = "^1.0.0"
commit = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
deps = []
"#;
        let lf = LockFile::parse(lf_content).unwrap();
        let result = verify_graph_completeness(&mf, &lf);
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("orphaned"),
            "expected orphan error, got: {msg}"
        );
    }

    #[test]
    fn test_verify_graph_completeness_ok() {
        let mf = ModFile::parse(
            "module github.com/acme/app\nvo ^1.0.0\nrequire github.com/acme/lib ^1.0.0\n",
        )
        .unwrap();
        let lf_content = r#"version = 1
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[resolved]]
path = "github.com/acme/lib"
version = "v1.0.0"
vo = "^1.0.0"
commit = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
deps = []
"#;
        let lf = LockFile::parse(lf_content).unwrap();
        assert!(verify_graph_completeness(&mf, &lf).is_ok());
    }

    #[test]
    fn test_locked_module_from_manifest_raw() {
        let manifest = sample_manifest();
        let raw = manifest.render();
        let locked = locked_module_from_manifest_raw(&manifest, raw.as_bytes());
        assert_eq!(locked.path.as_str(), "github.com/acme/lib");
        assert_eq!(locked.version.to_string(), "v1.2.3");
        assert_eq!(locked.deps.len(), 1);
        assert_eq!(locked.artifacts.len(), 1);
        assert_eq!(locked.release_manifest, Digest::from_sha256(raw.as_bytes()));
    }

    #[test]
    fn test_validate_locked_module_against_manifest_detects_commit_mismatch() {
        let manifest = sample_manifest();
        let raw = manifest.render();
        let digest = Digest::from_sha256(raw.as_bytes());
        let mut locked = locked_module_from_manifest_raw(&manifest, raw.as_bytes());
        locked.commit = "ffffffffffffffffffffffffffffffffffffffff".to_string();
        let err = validate_locked_module_against_manifest(&locked, &manifest, &digest).unwrap_err();
        assert!(err.to_string().contains("commit"));
    }
}
