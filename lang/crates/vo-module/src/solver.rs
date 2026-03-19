use std::collections::{BTreeMap, BTreeSet};

use crate::identity::ModulePath;
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::registry::Registry;
use crate::schema::manifest::ReleaseManifest;
use crate::Error;

/// The output of dependency resolution: a complete module graph.
#[derive(Debug, Clone)]
pub struct ResolvedGraph {
    pub modules: BTreeMap<ModulePath, ResolvedModule>,
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub version: ExactVersion,
    pub manifest: ReleaseManifest,
    /// Raw bytes of the `vo.release.json` as fetched from the registry.
    /// Used to compute the `release_manifest` digest for `vo.lock` and
    /// to write the manifest file into the module cache.
    pub manifest_raw: Vec<u8>,
}

/// Optional preferences for targeted update (e.g. `vo mod update [module]`).
#[derive(Debug, Clone, Default)]
pub struct SolvePreferences {
    /// If set, only this module should be updated; others should prefer their
    /// currently locked versions when they still satisfy all constraints.
    pub target_update: Option<ModulePath>,
    /// Currently locked versions to prefer (from existing vo.lock).
    pub locked: BTreeMap<ModulePath, ExactVersion>,
}

/// Solve the dependency graph deterministically.
///
/// Algorithm:
/// 1. Start with root module's direct requirements.
/// 2. For each unresolved module, fetch candidate versions from registry.
/// 3. Filter to compatible versions (major-path rule + constraint satisfaction).
/// 4. Select the highest satisfying version (or locked version if preferred).
/// 5. Fetch manifest, add transitive dependencies.
/// 6. Repeat until graph is closed.
///
/// Single-version rule: each module path appears at most once.
pub fn solve(
    root_module: &ModulePath,
    root_vo: &ToolchainConstraint,
    root_requires: &[(ModulePath, DepConstraint)],
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<ResolvedGraph, Error> {
    let mut resolved: BTreeMap<ModulePath, ResolvedModule> = BTreeMap::new();
    // Constraints accumulated for each module from all dependents
    let mut constraints: BTreeMap<ModulePath, Vec<(String, DepConstraint)>> = BTreeMap::new();
    // Work queue of modules to resolve
    let mut queue: Vec<ModulePath> = Vec::new();
    let mut queued: BTreeSet<ModulePath> = BTreeSet::new();

    // Seed with root's direct requirements
    for (mp, constraint) in root_requires {
        constraints
            .entry(mp.clone())
            .or_default()
            .push((root_module.as_str().to_string(), constraint.clone()));
        if queued.insert(mp.clone()) {
            queue.push(mp.clone());
        }
    }

    while let Some(mp) = queue.pop() {
        if resolved.contains_key(&mp) {
            continue;
        }

        let active_constraints = constraints.get(&mp).cloned().unwrap_or_default();

        // Determine the version to select
        let version = select_version(&mp, &active_constraints, registry, prefs)?;

        // Fetch and validate manifest
        let (manifest, manifest_raw) = registry.fetch_manifest_raw(&mp, &version)?;
        crate::registry::validate_manifest(&manifest, &mp, &version)?;

        // Toolchain constraint subset check: the root's `vo` constraint must be
        // a subset of the dependency's `vo` constraint.  If it isn't, the
        // dependency might require a toolchain version the root cannot provide.
        if !root_vo.is_subset_of(&manifest.vo) {
            return Err(Error::DependencyToolchainMismatch {
                module: mp.as_str().to_string(),
                project_constraint: root_vo.to_string(),
                dependency_constraint: manifest.vo.to_string(),
            });
        }

        // Add transitive dependencies to the queue
        for req in &manifest.require {
            let dep_mp = req.module.clone();
            constraints
                .entry(dep_mp.clone())
                .or_default()
                .push((mp.as_str().to_string(), req.constraint.clone()));
            if !resolved.contains_key(&dep_mp) && queued.insert(dep_mp.clone()) {
                queue.push(dep_mp);
            }
        }

        // If module was already resolved by a different path through the graph,
        // verify the selected version still satisfies all (now-expanded) constraints.
        // (In this algorithm we resolve before re-visiting, so this is the first resolution.)
        resolved.insert(mp, ResolvedModule { version, manifest, manifest_raw });
    }

    // Final validation pass: verify all constraints are satisfied by selected versions
    for (mp, cs) in &constraints {
        let rm = resolved.get(mp).ok_or_else(|| {
            Error::NoSatisfyingVersion {
                module: mp.as_str().to_string(),
                detail: "module was required but never resolved".to_string(),
            }
        })?;
        for (source, constraint) in cs {
            if !constraint.satisfies(&rm.version) {
                return Err(Error::ConflictingConstraints {
                    module: mp.as_str().to_string(),
                    detail: format!(
                        "selected {} but {} requires {}",
                        rm.version, source, constraint
                    ),
                });
            }
        }
    }

    Ok(ResolvedGraph { modules: resolved })
}

fn select_version(
    mp: &ModulePath,
    constraints: &[(String, DepConstraint)],
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<ExactVersion, Error> {
    // Check if we should prefer a locked version
    let should_prefer_locked = match &prefs.target_update {
        Some(target) => *target != *mp, // prefer locked for non-target modules
        None => false,
    };

    if should_prefer_locked {
        if let Some(locked_v) = prefs.locked.get(mp) {
            // Verify the locked version still satisfies all constraints
            let all_satisfied = constraints.iter().all(|(_, c)| c.satisfies(locked_v));
            if all_satisfied && mp.accepts_version(locked_v) {
                return Ok(locked_v.clone());
            }
            // Locked version no longer works; fall through to fresh selection.
        }
    }

    // Fetch candidate versions from registry
    let all_versions = registry.list_versions(mp)?;

    // Filter to major-compatible versions
    let compatible: Vec<ExactVersion> = crate::registry::filter_compatible_versions(mp, &all_versions);
    if compatible.is_empty() {
        return Err(Error::NoSatisfyingVersion {
            module: mp.as_str().to_string(),
            detail: "no compatible versions found in registry".to_string(),
        });
    }

    // Filter by all active constraints
    let satisfying: Vec<&ExactVersion> = compatible
        .iter()
        .filter(|v| constraints.iter().all(|(_, c)| c.satisfies(v)))
        .collect();

    if satisfying.is_empty() {
        let detail = constraints
            .iter()
            .map(|(src, c)| format!("  {src} requires: {c}"))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(Error::NoSatisfyingVersion {
            module: mp.as_str().to_string(),
            detail,
        });
    }

    // Select highest version (deterministic: Ord on SemVer)
    let selected = satisfying.into_iter().max().unwrap();
    Ok(selected.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::manifest::{ManifestSource, ReleaseManifest};
    use std::collections::HashMap;

    struct MockRegistry {
        versions: HashMap<String, Vec<ExactVersion>>,
        manifests: HashMap<(String, String), ReleaseManifest>,
    }

    impl MockRegistry {
        fn new() -> Self {
            Self {
                versions: HashMap::new(),
                manifests: HashMap::new(),
            }
        }

        fn add_module(&mut self, path: &str, ver: &str, deps: &[(&str, &str)]) {
            self.add_module_with_vo(path, ver, "^1.0.0", deps);
        }

        fn add_module_with_vo(&mut self, path: &str, ver: &str, vo: &str, deps: &[(&str, &str)]) {
            let mp = ModulePath::parse(path).unwrap();
            let ev = ExactVersion::parse(ver).unwrap();
            self.versions
                .entry(path.to_string())
                .or_default()
                .push(ev.clone());

            let require: Vec<crate::schema::manifest::ManifestRequire> = deps
                .iter()
                .map(|(m, c)| crate::schema::manifest::ManifestRequire {
                    module: ModulePath::parse(m).unwrap(),
                    constraint: DepConstraint::parse(c).unwrap(),
                })
                .collect();

            let manifest = ReleaseManifest {
                schema_version: 1,
                module: mp.clone(),
                version: ev.clone(),
                commit: "a".repeat(40),
                module_root: mp.module_root().to_string(),
                vo: crate::version::ToolchainConstraint::parse(vo).unwrap(),
                require,
                source: ManifestSource {
                    name: "source.tar.gz".into(),
                    size: 100,
                    digest: crate::digest::Digest::parse(
                        "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                    )
                    .unwrap(),
                },
                artifacts: vec![],
            };
            self.manifests
                .insert((path.to_string(), ver.to_string()), manifest);
        }
    }

    impl Registry for MockRegistry {
        fn list_versions(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
            Ok(self
                .versions
                .get(module.as_str())
                .cloned()
                .unwrap_or_default())
        }

        fn fetch_manifest(
            &self,
            module: &ModulePath,
            version: &ExactVersion,
        ) -> Result<ReleaseManifest, Error> {
            self.manifests
                .get(&(module.as_str().to_string(), version.to_string()))
                .cloned()
                .ok_or_else(|| {
                    Error::RegistryError(format!("no manifest for {} {}", module, version))
                })
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("not implemented in mock".into()))
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("not implemented in mock".into()))
        }
    }

    #[test]
    fn test_solve_simple() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/lib", "v1.2.0", &[]);
        reg.add_module("github.com/acme/lib", "v1.3.0", &[]);

        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];

        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let graph = solve(&root, &root_vo, &reqs, &reg, &SolvePreferences::default()).unwrap();
        assert_eq!(graph.modules.len(), 1);
        let lib = &graph.modules[&ModulePath::parse("github.com/acme/lib").unwrap()];
        assert_eq!(lib.version.to_string(), "v1.3.0"); // highest
    }

    #[test]
    fn test_solve_transitive() {
        let mut reg = MockRegistry::new();
        reg.add_module(
            "github.com/acme/lib",
            "v1.0.0",
            &[("github.com/acme/util", "^1.0.0")],
        );
        reg.add_module("github.com/acme/util", "v1.0.0", &[]);
        reg.add_module("github.com/acme/util", "v1.1.0", &[]);

        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];

        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let graph = solve(&root, &root_vo, &reqs, &reg, &SolvePreferences::default()).unwrap();
        assert_eq!(graph.modules.len(), 2);
        let util = &graph.modules[&ModulePath::parse("github.com/acme/util").unwrap()];
        assert_eq!(util.version.to_string(), "v1.1.0"); // highest
    }

    #[test]
    fn test_solve_no_version() {
        let reg = MockRegistry::new();
        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];
        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        assert!(solve(&root, &root_vo, &reqs, &reg, &SolvePreferences::default()).is_err());
    }

    #[test]
    fn test_solve_prefer_locked() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/lib", "v1.0.0", &[]);
        reg.add_module("github.com/acme/lib", "v1.1.0", &[]);
        reg.add_module("github.com/acme/other", "v1.0.0", &[]);
        reg.add_module("github.com/acme/other", "v1.2.0", &[]);

        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let reqs = vec![
            (
                ModulePath::parse("github.com/acme/lib").unwrap(),
                DepConstraint::parse("^1.0.0").unwrap(),
            ),
            (
                ModulePath::parse("github.com/acme/other").unwrap(),
                DepConstraint::parse("^1.0.0").unwrap(),
            ),
        ];

        let mut locked = BTreeMap::new();
        locked.insert(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            ExactVersion::parse("v1.0.0").unwrap(),
        );
        locked.insert(
            ModulePath::parse("github.com/acme/other").unwrap(),
            ExactVersion::parse("v1.0.0").unwrap(),
        );

        let prefs = SolvePreferences {
            target_update: Some(ModulePath::parse("github.com/acme/other").unwrap()),
            locked,
        };

        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let graph = solve(&root, &root_vo, &reqs, &reg, &prefs).unwrap();
        // lib should stay at locked v1.0.0
        let lib = &graph.modules[&ModulePath::parse("github.com/acme/lib").unwrap()];
        assert_eq!(lib.version.to_string(), "v1.0.0");
        // other should get highest (it's the target)
        let other = &graph.modules[&ModulePath::parse("github.com/acme/other").unwrap()];
        assert_eq!(other.version.to_string(), "v1.2.0");
    }

    #[test]
    fn test_solve_toolchain_mismatch() {
        let mut reg = MockRegistry::new();
        // Dependency requires ~2.0.0 but root requires ^1.0.0 — root is not subset of ~2.0.0.
        reg.add_module_with_vo("github.com/acme/lib", "v1.0.0", "~2.0.0", &[]);

        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];
        let result = solve(&root, &root_vo, &reqs, &reg, &SolvePreferences::default());
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("toolchain"), "expected toolchain error, got: {msg}");
    }

    #[test]
    fn test_solve_toolchain_subset_ok() {
        let mut reg = MockRegistry::new();
        // Root ^1.0.0 is subset of dep ^1.0.0 — should succeed.
        reg.add_module_with_vo("github.com/acme/lib", "v1.0.0", "^1.0.0", &[]);

        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];
        let graph = solve(&root, &root_vo, &reqs, &reg, &SolvePreferences::default()).unwrap();
        assert_eq!(graph.modules.len(), 1);
    }
}
