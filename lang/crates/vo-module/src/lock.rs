use std::collections::{BTreeMap, BTreeSet};

use crate::digest::Digest;
use crate::ext_manifest::{DeclaredArtifactId, ExtensionManifest};
use crate::identity::ModulePath;
use crate::schema::lockfile::{
    validate_locked_module_graph, validate_locked_toolchain_coverage, LockFile, LockRoot,
    LockedArtifact, LockedModule, LockedRequirement, LOCK_FILE_VERSION,
};
use crate::schema::manifest::ReleaseManifest;
use crate::schema::modfile::ModFile;
use crate::solver::ResolvedGraph;
use crate::Error;

fn manifest_deps(manifest: &ReleaseManifest) -> Vec<LockedRequirement> {
    let mut deps = manifest
        .require
        .iter()
        .map(|require| LockedRequirement {
            module: require.module.clone(),
            constraint: require.constraint.clone(),
        })
        .collect::<Vec<_>>();
    deps.sort_by(|a, b| a.module.cmp(&b.module));
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

fn manifest_declared_artifact_ids(manifest: &ReleaseManifest) -> Vec<DeclaredArtifactId> {
    let mut artifacts = manifest
        .artifacts
        .iter()
        .map(|artifact| DeclaredArtifactId {
            kind: artifact.id.kind.clone(),
            target: artifact.id.target.clone(),
            name: artifact.id.name.clone(),
        })
        .collect::<Vec<_>>();
    artifacts.sort();
    artifacts
}

fn format_deps(deps: &[LockedRequirement]) -> String {
    deps.iter()
        .map(|dep| format!("{} {}", dep.module, dep.constraint))
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_declared_artifacts(artifacts: &[DeclaredArtifactId]) -> String {
    artifacts
        .iter()
        .map(|artifact| format!("{} {} {}", artifact.kind, artifact.target, artifact.name,))
        .collect::<Vec<_>>()
        .join(", ")
}

pub(crate) fn validate_extension_manifest_against_release_manifest(
    ext_manifest: Option<&ExtensionManifest>,
    manifest: &ReleaseManifest,
) -> Result<(), Error> {
    if let Some(ext_manifest) = ext_manifest {
        ext_manifest.validate()?;
    }
    let declared = ext_manifest
        .map(ExtensionManifest::declared_artifact_ids)
        .unwrap_or_default()
        .into_iter()
        .collect::<BTreeSet<_>>();
    let published = manifest_declared_artifact_ids(manifest)
        .into_iter()
        .collect::<BTreeSet<_>>();

    let missing = declared.difference(&published).cloned().collect::<Vec<_>>();
    let undeclared = published.difference(&declared).cloned().collect::<Vec<_>>();
    if missing.is_empty() && undeclared.is_empty() {
        return Ok(());
    }

    let mut detail = Vec::new();
    if !missing.is_empty() {
        detail.push(format!(
            "missing declared artifacts [{}]",
            format_declared_artifacts(&missing)
        ));
    }
    if !undeclared.is_empty() {
        detail.push(format!(
            "undeclared published artifacts [{}]",
            format_declared_artifacts(&undeclared)
        ));
    }
    Err(Error::InvalidReleaseMetadata(format!(
        "artifact contract mismatch for {}@{} between packaged vo.mod and vo.release.json: {}",
        manifest.module,
        manifest.version,
        detail.join("; "),
    )))
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

fn locked_module_from_release_manifest(
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

pub(crate) fn locked_module_from_manifest_raw(
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
pub(crate) fn generate_lock(
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

    let lock_file = LockFile {
        version: LOCK_FILE_VERSION,
        created_by: created_by.to_string(),
        root,
        resolved,
    };
    lock_file.render()?;
    verify_root_consistency(root_mod, &lock_file)?;
    verify_graph_completeness(root_mod, &lock_file)?;
    Ok(lock_file)
}

/// Verify that root `vo.mod` and root `vo.lock` are consistent.
/// Per spec §12.1: root.module and root.vo MUST exactly match.
pub(crate) fn verify_root_consistency(
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<(), Error> {
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

/// Verify the complete selected graph against the root requirements.
///
/// Every root and transitive edge must be present and satisfied, and every
/// selected module must be reachable from the root. Workspace source overrides
/// deliberately do not alter this authority graph.
pub(crate) fn verify_graph_completeness(
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<(), Error> {
    validate_locked_module_graph(&lock_file.resolved)?;

    let edge_count = lock_file
        .resolved
        .iter()
        .try_fold(mod_file.require.len(), |total, module| {
            total.checked_add(module.deps.len())
        });
    if edge_count.is_none_or(|count| count > crate::MAX_SOLVER_GRAPH_EDGES) {
        return Err(Error::ResolutionLimitExceeded {
            resource: "root lock graph edge count".to_string(),
            limit: crate::MAX_SOLVER_GRAPH_EDGES,
        });
    }

    validate_locked_toolchain_coverage(&lock_file.root.vo, &lock_file.resolved)?;

    // Validation above proves path uniqueness. Keep one ordered index for all
    // root and traversal lookups so large valid graphs remain O((V + E) log V).
    let selected: BTreeMap<&ModulePath, &LockedModule> = lock_file
        .resolved
        .iter()
        .map(|module| (&module.path, module))
        .collect();

    // Every direct requirement must have a resolved entry that satisfies vo.mod.
    for req in &mod_file.require {
        let locked = selected.get(&req.module).copied().ok_or_else(|| {
            Error::LockFileParse(format!(
                "vo.mod requires {} but it is not in vo.lock",
                req.module
            ))
        })?;
        if !req.constraint.satisfies(&locked.version) {
            return Err(Error::LockFileParse(format!(
                "vo.mod requires {} {} but vo.lock pins {} {}",
                req.module, req.constraint, locked.path, locked.version
            )));
        }
    }

    // Every resolved module must be reachable from root's direct requirements,
    // and every dep listed in a locked module must itself be present in the lock.
    let mut reachable = BTreeSet::new();
    let mut stack: Vec<ModulePath> = mod_file
        .require
        .iter()
        .map(|require| require.module.clone())
        .collect();
    while let Some(mp) = stack.pop() {
        if !reachable.insert(mp.clone()) {
            continue;
        }
        let lm = selected.get(&mp).copied().ok_or_else(|| {
            Error::LockFileParse(format!(
                "transitive dependency {} is referenced but not present in vo.lock",
                mp
            ))
        })?;
        for dep in &lm.deps {
            stack.push(dep.module.clone());
        }
    }
    for lm in &lock_file.resolved {
        if !reachable.contains(&lm.path) {
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
    use std::path::Path;

    use crate::ext_manifest::parse_ext_manifest_content;

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
    "digest": "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    "files_size": 37,
    "files_digest": "sha256:04e07c8d1db1df7c86bc43c5ff9672b6622e7d7b5fef22b4397ca6588073aca5"
  },
  "web_manifest": {
    "size": 3,
    "digest": "sha256:ca3d163bab055381827226140568f3bef7eaac187cebd76878e0b63e9e442356"
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

    fn sample_bindgen_ext_manifest() -> ExtensionManifest {
        parse_ext_manifest_content(
            concat!(
                "[extension]\n",
                "name = \"lib\"\n\n",
                "[extension.wasm]\n",
                "type = \"bindgen\"\n",
                "wasm = \"lib.wasm\"\n",
                "js_glue = \"lib.js\"\n",
            ),
            Path::new("vo.mod"),
        )
        .unwrap()
    }

    #[test]
    fn test_verify_root_consistency_ok() {
        let mf = ModFile::parse("module github.com/acme/app\nvo ^1.0.0\n").unwrap();
        let lf_content = r#"version = 2
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
        let lf_content = r#"version = 2
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
        // lib declares a dep on util, but util is NOT in the lock file.
        let lf_content = r#"version = 2
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
deps = [
  { module = "github.com/acme/util", constraint = "^1.0.0" },
]
"#;
        let result = LockFile::parse(lf_content);
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("github.com/acme/util"),
            "expected error about missing util, got: {msg}"
        );
    }

    #[test]
    fn test_verify_graph_completeness_rejects_stale_direct_requirement_version() {
        let mf = ModFile::parse(
            "module github.com/acme/app\nvo ^1.0.0\nrequire github.com/acme/lib ^1.1.0\n",
        )
        .unwrap();
        let lf_content = r#"version = 2
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
        let result = verify_graph_completeness(&mf, &lf);
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("vo.mod requires github.com/acme/lib ^1.1.0 but vo.lock pins github.com/acme/lib v1.0.0"),
            "expected stale lock error, got: {msg}"
        );
    }

    #[test]
    fn test_verify_graph_completeness_rejects_dependency_toolchain_mismatch() {
        let mf = ModFile::parse(
            "module github.com/acme/app\nvo ^1.0.0\nrequire github.com/acme/lib ^1.0.0\n",
        )
        .unwrap();
        let lf_content = r#"version = 2
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
        let mut lf = LockFile::parse(lf_content).unwrap();
        lf.resolved[0].vo = crate::version::ToolchainConstraint::parse("^2.0.0").unwrap();
        let error = verify_graph_completeness(&mf, &lf).unwrap_err();
        assert!(matches!(error, Error::DependencyToolchainMismatch { .. }));
        assert!(error.to_string().contains("github.com/acme/lib"));
    }

    #[test]
    fn test_verify_graph_completeness_orphaned_module() {
        let mf = ModFile::parse("module github.com/acme/app\nvo ^1.0.0\n").unwrap();
        // Lock has a resolved module that nobody depends on.
        let lf_content = r#"version = 2
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
        let lf_content = r#"version = 2
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
    fn test_verify_graph_completeness_requires_workspace_overridden_direct_modules() {
        let mf = ModFile::parse(
            "module github.com/acme/app\nvo ^1.0.0\nrequire github.com/acme/lib ^1.0.0\n",
        )
        .unwrap();
        let lf_content = r#"version = 2
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/app"
vo = "^1.0.0"
"#;
        let lf = LockFile::parse(lf_content).unwrap();
        let error = verify_graph_completeness(&mf, &lf).unwrap_err();
        assert!(error.to_string().contains("is not in vo.lock"));
    }

    #[test]
    fn test_verify_graph_completeness_keeps_transitive_lock_coverage_through_workspace_override() {
        let mf = ModFile::parse(
            "module github.com/acme/app\nvo ^1.0.0\nrequire github.com/acme/lib ^1.0.0\n",
        )
        .unwrap();
        let lf_content = r#"version = 2
created_by = "vo 1.0.0"
[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[resolved]]
path = "github.com/acme/core"
version = "v1.0.0"
vo = "^1.0.0"
commit = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
release_manifest = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
source = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
deps = []

[[resolved]]
path = "github.com/acme/lib"
version = "v1.0.0"
vo = "^1.0.0"
commit = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
release_manifest = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
source = "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
deps = [
  { module = "github.com/acme/core", constraint = "^1.0.0" },
]
"#;
        let lf = LockFile::parse(lf_content).unwrap();
        assert!(verify_graph_completeness(&mf, &lf).is_ok());
    }

    #[test]
    fn test_locked_module_from_manifest_raw() {
        let manifest = sample_manifest();
        let raw = manifest.render().unwrap();
        let locked = locked_module_from_manifest_raw(&manifest, raw.as_bytes());
        assert_eq!(locked.path.as_str(), "github.com/acme/lib");
        assert_eq!(locked.version.to_string(), "v1.2.3");
        assert_eq!(locked.deps.len(), 1);
        assert_eq!(locked.deps[0].module.as_str(), "github.com/acme/util");
        assert_eq!(locked.deps[0].constraint.to_string(), "^0.1.0");
        assert_eq!(locked.artifacts.len(), 1);
        assert_eq!(locked.release_manifest, Digest::from_sha256(raw.as_bytes()));
    }

    #[test]
    fn release_manifest_lock_digest_binds_web_manifest_metadata() {
        let manifest = sample_manifest();
        let raw = manifest.render().unwrap();
        let locked = locked_module_from_manifest_raw(&manifest, raw.as_bytes());

        let mut changed = manifest.clone();
        changed.web_manifest.digest = Digest::from_sha256(b"different vo.web.json bytes");
        let changed_raw = changed.render().unwrap();
        let changed_locked = locked_module_from_manifest_raw(&changed, changed_raw.as_bytes());

        assert_ne!(locked.release_manifest, changed_locked.release_manifest);
        assert_eq!(locked.source, changed_locked.source);
        assert_eq!(locked.artifacts.len(), changed_locked.artifacts.len());
        for (left, right) in locked.artifacts.iter().zip(&changed_locked.artifacts) {
            assert_eq!(left.id, right.id);
            assert_eq!(left.size, right.size);
            assert_eq!(left.digest, right.digest);
        }
    }

    #[test]
    fn test_validate_locked_module_against_manifest_detects_commit_mismatch() {
        let manifest = sample_manifest();
        let raw = manifest.render().unwrap();
        let digest = Digest::from_sha256(raw.as_bytes());
        let mut locked = locked_module_from_manifest_raw(&manifest, raw.as_bytes());
        locked.commit = "ffffffffffffffffffffffffffffffffffffffff".to_string();
        let err = validate_locked_module_against_manifest(&locked, &manifest, &digest).unwrap_err();
        assert!(err.to_string().contains("commit"));
    }

    #[test]
    fn test_validate_locked_module_against_manifest_detects_dependency_constraint_mismatch() {
        let manifest = sample_manifest();
        let raw = manifest.render().unwrap();
        let digest = Digest::from_sha256(raw.as_bytes());
        let mut locked = locked_module_from_manifest_raw(&manifest, raw.as_bytes());
        locked.deps[0].constraint = crate::version::DepConstraint::parse("~0.1.0").unwrap();

        let error =
            validate_locked_module_against_manifest(&locked, &manifest, &digest).unwrap_err();
        assert!(matches!(error, Error::LockedModuleMismatch { .. }));
        assert!(error.to_string().contains("deps"));
        assert!(error.to_string().contains("^0.1.0"));
        assert!(error.to_string().contains("~0.1.0"));
    }

    #[test]
    fn test_validate_extension_manifest_against_release_manifest_rejects_missing_declared_artifact()
    {
        let manifest = sample_manifest();
        let ext_manifest = sample_bindgen_ext_manifest();

        let err =
            validate_extension_manifest_against_release_manifest(Some(&ext_manifest), &manifest)
                .unwrap_err();

        assert!(err.to_string().contains("missing declared artifacts"));
        assert!(err
            .to_string()
            .contains("extension-js-glue wasm32-unknown-unknown lib.js"));
    }

    #[test]
    fn test_validate_extension_manifest_against_release_manifest_rejects_undeclared_published_artifact(
    ) {
        let manifest = sample_manifest();

        let err =
            validate_extension_manifest_against_release_manifest(None, &manifest).unwrap_err();

        assert!(err.to_string().contains("undeclared published artifacts"));
        assert!(err
            .to_string()
            .contains("extension-wasm wasm32-unknown-unknown lib.wasm"));
    }

    #[test]
    fn test_validate_extension_manifest_against_release_manifest_accepts_matching_contract() {
        let manifest = ReleaseManifest::parse(
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
    "files_size": 37,
    "files_digest": "sha256:04e07c8d1db1df7c86bc43c5ff9672b6622e7d7b5fef22b4397ca6588073aca5"
  },
  "web_manifest": {
    "size": 3,
    "digest": "sha256:ca3d163bab055381827226140568f3bef7eaac187cebd76878e0b63e9e442356"
  },
  "artifacts": [
    {
      "kind": "extension-js-glue",
      "target": "wasm32-unknown-unknown",
      "name": "lib.js",
      "size": 5,
      "digest": "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    },
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
        .unwrap();
        let ext_manifest = sample_bindgen_ext_manifest();

        validate_extension_manifest_against_release_manifest(Some(&ext_manifest), &manifest)
            .unwrap();
    }
}
