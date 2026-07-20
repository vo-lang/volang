use std::collections::BTreeSet;

use crate::digest::Digest;
use crate::ext_manifest::{DeclaredArtifactId, ExtensionManifest};
use crate::schema::lockfile::{
    validate_locked_module_graph, LockFile, LockOrigin, LockedModule, LOCK_FILE_VERSION,
};
use crate::schema::manifest::ReleaseManifest;
use crate::schema::modfile::ModFile;
use crate::solver::ResolvedGraph;
use crate::Error;

/// Compute the typed authored-intent identity used by locks, releases, and
/// workspace overlays. Local producer paths never participate.
pub fn module_intent_digest(mod_file: &ModFile) -> Result<Digest, Error> {
    let mut public = mod_file.clone();
    if let Some(extension) = public.extension.as_mut() {
        extension.build = None;
    }
    let canonical = public.render()?;
    let mut bytes = Vec::with_capacity("vo-module-intent-v1\0".len() + canonical.len());
    bytes.extend_from_slice(b"vo-module-intent-v1\0");
    bytes.extend_from_slice(canonical.as_bytes());
    Ok(Digest::from_sha256(&bytes))
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

fn format_declared_artifacts(artifacts: &[DeclaredArtifactId]) -> String {
    crate::summarize_diagnostic_items(
        artifacts
            .iter()
            .map(|artifact| format!("{} {} {}", artifact.kind, artifact.target, artifact.name)),
        "artifact(s)",
    )
}

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
            format_declared_artifacts(&missing)
        ));
    }
    if !undeclared.is_empty() {
        details.push(format!(
            "undeclared published artifacts [{}]",
            format_declared_artifacts(&undeclared)
        ));
    }
    Err(Error::InvalidReleaseMetadata(format!(
        "artifact contract mismatch for {}@{}: {}",
        release.module,
        release.version,
        details.join("; ")
    )))
}

fn locked_module_from_release_manifest(
    release: &ReleaseManifest,
    release_digest: Digest,
) -> LockedModule {
    LockedModule {
        path: release.module.clone(),
        version: release.version.clone(),
        origin: LockOrigin::Registry,
        release: Some(release_digest),
        intent: None,
    }
}

pub(crate) fn locked_module_from_manifest_raw(
    release: &ReleaseManifest,
    release_raw: &[u8],
) -> LockedModule {
    locked_module_from_release_manifest(release, Digest::from_sha256(release_raw))
}

pub fn validate_locked_module_against_manifest(
    locked: &LockedModule,
    release: &ReleaseManifest,
    release_digest: &Digest,
) -> Result<(), Error> {
    let expected = locked_module_from_release_manifest(release, release_digest.clone());
    for (field, expected, found) in [
        ("path", expected.path.to_string(), locked.path.to_string()),
        (
            "version",
            expected.version.to_string(),
            locked.version.to_string(),
        ),
        (
            "origin",
            "registry".to_string(),
            match locked.origin {
                LockOrigin::Registry => "registry",
                LockOrigin::Workspace => "workspace",
            }
            .to_string(),
        ),
        (
            "release",
            expected.release.as_ref().expect("release").to_string(),
            locked
                .release
                .as_ref()
                .map(ToString::to_string)
                .unwrap_or_else(|| "<missing>".to_string()),
        ),
    ] {
        if expected != found {
            return Err(Error::LockedModuleMismatch {
                module: locked.path.to_string(),
                field: field.to_string(),
                expected,
                found,
            });
        }
    }
    Ok(())
}

pub(crate) fn generate_lock(root_mod: &ModFile, graph: &ResolvedGraph) -> Result<LockFile, Error> {
    if graph.modules.is_empty() {
        return Err(Error::LockFileParse(
            "cannot generate vo.lock for a dependency-free module".to_string(),
        ));
    }
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
        format: LOCK_FILE_VERSION,
        root: module_intent_digest(root_mod)?,
        modules,
    };
    verify_graph_completeness(root_mod, &lock_file)?;
    Ok(lock_file)
}

pub(crate) fn verify_root_consistency(
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<(), Error> {
    let intent = module_intent_digest(mod_file)?;
    if intent != lock_file.root {
        return Err(Error::RootMismatch {
            field: "intent".to_string(),
            mod_value: intent.to_string(),
            lock_value: lock_file.root.to_string(),
        });
    }
    Ok(())
}

/// Verify the root selection without recreating descriptor edges in `vo.lock`.
/// Complete closure validation happens when the exact release/workspace intent
/// descriptors are loaded into a ProjectPlan.
pub(crate) fn verify_graph_completeness(
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<(), Error> {
    validate_locked_module_graph(&lock_file.modules)?;
    verify_root_consistency(mod_file, lock_file)?;
    for dependency in &mod_file.dependencies {
        let locked = lock_file.find(&dependency.module).ok_or_else(|| {
            Error::LockFileParse(format!(
                "vo.mod depends on {} but it is absent from vo.lock",
                dependency.module
            ))
        })?;
        if !dependency.constraint.satisfies(&locked.version) {
            return Err(Error::LockFileParse(format!(
                "vo.mod depends on {} {} but vo.lock selects {}",
                dependency.module, dependency.constraint, locked.version
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intent_digest_ignores_local_build_adapter_paths() {
        let base = r#"format = 1
module = "example.com/acme/render"
version = "0.2.0"
vo = "0.1.4"

[extension]
name = "render"

[extension.native]
targets = ["aarch64-apple-darwin"]

[build.native]
kind = "prebuilt"
path = "target/a.dylib"
"#;
        let changed = base.replace("target/a.dylib", "out/render.dylib");
        let left = ModFile::parse(base).unwrap();
        let right = ModFile::parse(&changed).unwrap();
        assert_eq!(
            module_intent_digest(&left).unwrap(),
            module_intent_digest(&right).unwrap()
        );
    }
}
