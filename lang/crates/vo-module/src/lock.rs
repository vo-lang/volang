use std::collections::{BTreeMap, BTreeSet};

use crate::digest::Digest;
use crate::ext_manifest::{DeclaredArtifactId, ExtensionManifest};
use crate::identity::ModulePath;
use crate::schema::lockfile::{
    validate_locked_module_graph, validate_locked_toolchain_coverage, LockFile, LockRoot,
    LockedDependency, LockedModule, LOCK_FILE_VERSION,
};
use crate::schema::manifest::ReleaseManifest;
use crate::schema::modfile::ModFile;
use crate::solver::ResolvedGraph;
use crate::Error;

fn manifest_dependencies(manifest: &ReleaseManifest) -> Vec<LockedDependency> {
    let mut dependencies = manifest
        .dependencies
        .iter()
        .map(|dependency| LockedDependency {
            module: dependency.module.clone(),
            constraint: dependency.constraint.clone(),
        })
        .collect::<Vec<_>>();
    dependencies.sort_by(|left, right| left.module.cmp(&right.module));
    dependencies
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

fn format_dependencies(dependencies: &[LockedDependency]) -> String {
    crate::summarize_diagnostic_items(
        dependencies
            .iter()
            .map(|dependency| format!("{} {}", dependency.module, dependency.constraint)),
        "dependency entry/entries",
    )
}

fn format_declared_artifacts(artifacts: &[DeclaredArtifactId]) -> String {
    crate::summarize_diagnostic_items(
        artifacts
            .iter()
            .map(|artifact| format!("{} {} {}", artifact.kind, artifact.target, artifact.name)),
        "artifact(s)",
    )
}

/// Check the public extension contract in authenticated `vo.mod` against the
/// artifact identities published by the authenticated release object.
pub(crate) fn validate_extension_manifest_against_release_manifest(
    extension: Option<&ExtensionManifest>,
    release: &ReleaseManifest,
) -> Result<(), Error> {
    if let Some(extension) = extension {
        extension.validate()?;
    }
    let declared = extension
        .map(ExtensionManifest::declared_artifact_ids)
        .unwrap_or_default()
        .into_iter()
        .collect::<BTreeSet<_>>();
    let published = manifest_declared_artifact_ids(release)
        .into_iter()
        .collect::<BTreeSet<_>>();

    let missing = declared.difference(&published).cloned().collect::<Vec<_>>();
    let undeclared = published.difference(&declared).cloned().collect::<Vec<_>>();
    if missing.is_empty() && undeclared.is_empty() {
        return Ok(());
    }

    let mut details = Vec::new();
    if !missing.is_empty() {
        details.push(format!(
            "missing declared artifacts [{}]",
            format_declared_artifacts(&missing),
        ));
    }
    if !undeclared.is_empty() {
        details.push(format!(
            "undeclared published artifacts [{}]",
            format_declared_artifacts(&undeclared),
        ));
    }
    Err(Error::InvalidReleaseMetadata(format!(
        "artifact contract mismatch for {}@{} between packaged vo.mod and vo.release.json: {}",
        release.module,
        release.version,
        details.join("; "),
    )))
}

fn locked_module_from_release_manifest(
    release: &ReleaseManifest,
    release_digest: Digest,
) -> LockedModule {
    LockedModule {
        path: release.module.clone(),
        version: release.version.clone(),
        vo: release.vo.clone(),
        release: release_digest,
        dependencies: manifest_dependencies(release),
    }
}

/// Derive the complete lock entry from the exact authenticated release bytes.
pub(crate) fn locked_module_from_manifest_raw(
    release: &ReleaseManifest,
    release_raw: &[u8],
) -> LockedModule {
    locked_module_from_release_manifest(release, Digest::from_sha256(release_raw))
}

/// Prove that a compact lock entry selects the supplied authenticated release.
pub fn validate_locked_module_against_manifest(
    locked: &LockedModule,
    release: &ReleaseManifest,
    release_digest: &Digest,
) -> Result<(), Error> {
    let expected = locked_module_from_release_manifest(release, release_digest.clone());
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
    if locked.release != expected.release {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "release".to_string(),
            expected: expected.release.to_string(),
            found: locked.release.to_string(),
        });
    }
    if locked.dependencies != expected.dependencies {
        return Err(Error::LockedModuleMismatch {
            module: locked.path.to_string(),
            field: "dependencies".to_string(),
            expected: format_dependencies(&expected.dependencies),
            found: format_dependencies(&locked.dependencies),
        });
    }
    Ok(())
}

/// Freeze one solved graph into the canonical compact v3 lock protocol.
pub(crate) fn generate_lock(root_mod: &ModFile, graph: &ResolvedGraph) -> Result<LockFile, Error> {
    if graph.modules.is_empty() {
        return Err(Error::LockFileParse(
            "cannot generate vo.lock for an empty external dependency graph".to_string(),
        ));
    }
    let root = LockRoot {
        module: root_mod.module.clone(),
        vo: root_mod.vo.clone(),
    };
    let mut modules = Vec::new();
    modules
        .try_reserve(graph.modules.len())
        .map_err(|_| Error::LockFileParse("failed to reserve locked modules".to_string()))?;
    for (module, resolved) in &graph.modules {
        let locked = locked_module_from_manifest_raw(&resolved.manifest, &resolved.manifest_raw);
        debug_assert_eq!(locked.path, *module);
        debug_assert_eq!(locked.version, resolved.version);
        modules.push(locked);
    }
    modules.sort_by(|left, right| left.path.cmp(&right.path));

    let lock_file = LockFile {
        version: LOCK_FILE_VERSION,
        root,
        modules,
    };
    verify_root_consistency(root_mod, &lock_file)?;
    verify_graph_completeness(root_mod, &lock_file)?;
    lock_file.render()?;
    Ok(lock_file)
}

/// Verify the root identity and toolchain authority shared by `vo.mod` and
/// `vo.lock`.
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

/// Verify root edges, the closed transitive graph, constraints, reachability,
/// and toolchain coverage without consulting the network or cache.
pub(crate) fn verify_graph_completeness(
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<(), Error> {
    validate_locked_module_graph(&lock_file.modules)?;

    let edge_count = lock_file
        .modules
        .iter()
        .try_fold(mod_file.dependencies.len(), |total, module| {
            total.checked_add(module.dependencies.len())
        });
    if edge_count.is_none_or(|count| count > crate::MAX_SOLVER_GRAPH_EDGES) {
        return Err(Error::ResolutionLimitExceeded {
            resource: "root lock graph edge count".to_string(),
            limit: crate::MAX_SOLVER_GRAPH_EDGES,
        });
    }
    validate_locked_toolchain_coverage(&lock_file.root.vo, &lock_file.modules)?;

    let selected = lock_file
        .modules
        .iter()
        .map(|module| (&module.path, module))
        .collect::<BTreeMap<&ModulePath, &LockedModule>>();

    for dependency in &mod_file.dependencies {
        let locked = selected.get(&dependency.module).copied().ok_or_else(|| {
            Error::LockFileParse(format!(
                "vo.mod depends on {} but it is absent from vo.lock",
                dependency.module,
            ))
        })?;
        if !dependency.constraint.satisfies(&locked.version) {
            return Err(Error::LockFileParse(format!(
                "vo.mod depends on {} {} but vo.lock selects {} {}",
                dependency.module, dependency.constraint, locked.path, locked.version,
            )));
        }
    }

    let mut reachable = BTreeSet::new();
    let mut stack = mod_file
        .dependencies
        .iter()
        .map(|dependency| dependency.module.clone())
        .collect::<Vec<_>>();
    while let Some(module) = stack.pop() {
        if !reachable.insert(module.clone()) {
            continue;
        }
        let locked = selected.get(&module).copied().ok_or_else(|| {
            Error::LockFileParse(format!(
                "transitive dependency {module} is referenced but absent from vo.lock",
            ))
        })?;
        stack.extend(
            locked
                .dependencies
                .iter()
                .map(|dependency| dependency.module.clone()),
        );
    }
    for locked in &lock_file.modules {
        if !reachable.contains(&locked.path) {
            return Err(Error::LockFileParse(format!(
                "vo.lock contains orphaned module: {}",
                locked.path,
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::manifest::{
        ManifestArtifact, ManifestDependency, ManifestPackage, ManifestSource,
    };
    use crate::schema::modfile::ModFile;
    use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};

    fn sample_release() -> ReleaseManifest {
        ReleaseManifest {
            schema_version: 2,
            module: ModulePath::parse("github.com/acme/lib").unwrap(),
            version: ExactVersion::parse("1.2.3").unwrap(),
            commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            dependencies: vec![ManifestDependency {
                module: ModulePath::parse("github.com/acme/util").unwrap(),
                constraint: DepConstraint::parse("^0.1.0").unwrap(),
            }],
            source: ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 3,
                digest: Digest::from_sha256(b"src"),
            },
            package: ManifestPackage {
                size: 3,
                digest: Digest::from_sha256(b"pkg"),
            },
            artifacts: Vec::new(),
        }
    }

    fn root_mod() -> ModFile {
        ModFile::parse(
            r#"module = "github.com/acme/app"
vo = "^0.1.0"

[dependencies]
"github.com/acme/lib" = "^1.0.0"
"#,
        )
        .unwrap()
    }

    #[test]
    fn lock_entry_contains_only_selection_authority() {
        let release = sample_release();
        let raw = release.render().unwrap().into_bytes();
        let locked = locked_module_from_manifest_raw(&release, &raw);

        assert_eq!(locked.path, release.module);
        assert_eq!(locked.version.to_string(), "1.2.3");
        assert_eq!(locked.release, Digest::from_sha256(&raw));
        assert_eq!(locked.dependencies.len(), 1);
        validate_locked_module_against_manifest(&locked, &release, &Digest::from_sha256(&raw))
            .unwrap();
    }

    #[test]
    fn release_digest_and_dependency_edges_are_authenticated() {
        let release = sample_release();
        let raw = release.render().unwrap().into_bytes();
        let mut locked = locked_module_from_manifest_raw(&release, &raw);
        locked.release = Digest::from_sha256(b"different");
        let error =
            validate_locked_module_against_manifest(&locked, &release, &Digest::from_sha256(&raw))
                .unwrap_err();
        assert!(
            matches!(error, Error::LockedModuleMismatch { ref field, .. } if field == "release")
        );

        let mut locked = locked_module_from_manifest_raw(&release, &raw);
        locked.dependencies[0].constraint = DepConstraint::parse("~0.1.0").unwrap();
        let error =
            validate_locked_module_against_manifest(&locked, &release, &Digest::from_sha256(&raw))
                .unwrap_err();
        assert!(
            matches!(error, Error::LockedModuleMismatch { ref field, .. } if field == "dependencies")
        );
    }

    #[test]
    fn graph_verification_rejects_missing_and_orphaned_nodes() {
        let mod_file = root_mod();
        let lock = LockFile {
            version: LOCK_FILE_VERSION,
            root: LockRoot {
                module: mod_file.module.clone(),
                vo: mod_file.vo.clone(),
            },
            modules: Vec::new(),
        };
        assert!(verify_graph_completeness(&mod_file, &lock)
            .unwrap_err()
            .to_string()
            .contains("absent"));

        let mut release = sample_release();
        release.dependencies.clear();
        let raw = release.render().unwrap().into_bytes();
        let mut lock = lock;
        lock.modules
            .push(locked_module_from_manifest_raw(&release, &raw));
        let orphan = LockedModule {
            path: ModulePath::parse("github.com/acme/orphan").unwrap(),
            version: ExactVersion::parse("0.2.0").unwrap(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            release: Digest::from_sha256(b"orphan"),
            dependencies: Vec::new(),
        };
        lock.modules.push(orphan);
        lock.modules
            .sort_by(|left, right| left.path.cmp(&right.path));
        let error = verify_graph_completeness(&mod_file, &lock).unwrap_err();
        assert!(error.to_string().contains("orphaned"), "{error}");
    }

    #[test]
    fn extension_contract_uses_release_artifact_identities() {
        let mod_file = ModFile::parse(
            r#"module = "github.com/acme/lib"
vo = "^0.1.0"

[extension]
name = "lib"

[extension.wasm]
kind = "standalone"
wasm = "lib.wasm"
"#,
        )
        .unwrap();
        let mut release = sample_release();
        assert!(validate_extension_manifest_against_release_manifest(
            mod_file.extension.as_ref(),
            &release,
        )
        .is_err());
        release.artifacts.push(ManifestArtifact {
            id: crate::identity::ArtifactId {
                kind: "extension-wasm".to_string(),
                target: "wasm32-unknown-unknown".to_string(),
                name: "lib.wasm".to_string(),
            },
            size: 4,
            digest: Digest::from_sha256(b"wasm"),
        });
        validate_extension_manifest_against_release_manifest(mod_file.extension.as_ref(), &release)
            .unwrap();
    }
}
