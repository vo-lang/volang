use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::schema::lockfile::{LockFile, LockRoot, LockedArtifact, LockedModule};
use crate::schema::modfile::ModFile;
use crate::solver::ResolvedGraph;
use crate::Error;

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
        let deps: Vec<ModulePath> = {
            let mut d: Vec<ModulePath> = rm
                .manifest
                .require
                .iter()
                .map(|r| r.module.clone())
                .collect();
            d.sort();
            d
        };

        let artifacts: Vec<LockedArtifact> = {
            let mut a: Vec<LockedArtifact> = rm
                .manifest
                .artifacts
                .iter()
                .map(|ma| LockedArtifact {
                    id: ma.id.clone(),
                    size: ma.size,
                    digest: ma.digest.clone(),
                })
                .collect();
            a.sort_by(|x, y| x.id.cmp(&y.id));
            a
        };

        let manifest_digest = Digest::from_sha256(&rm.manifest_raw);

        resolved.push(LockedModule {
            path: mp.clone(),
            version: rm.version.clone(),
            vo: rm.manifest.vo.clone(),
            commit: rm.manifest.commit.clone(),
            release_manifest: manifest_digest,
            source: rm.manifest.source.digest.clone(),
            deps,
            artifacts,
        });
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
}
