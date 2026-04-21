use std::collections::BTreeMap;

use crate::identity::ModulePath;
use crate::registry::Registry;
use crate::schema::manifest::ReleaseManifest;
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
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
/// `root_source` is a human-readable label for the root module used only for
/// attribution in conflict error messages (e.g. "github.com/acme/app" for a
/// published project, or "local/demo" for a toolchain-synthesized ephemeral
/// single-file module). It is never resolved, fetched, or compared against
/// any registry entry, so ephemeral roots with reserved `local/*` identities
/// can pass through unchanged.
///
/// Algorithm:
/// 1. Start with root module's direct requirements.
/// 2. For each unresolved module, fetch candidate versions from registry.
/// 3. Filter to compatible versions (major-path rule + constraint satisfaction).
/// 4. Filter out candidates whose release manifest toolchain constraint does not
///    support the root project's toolchain requirement.
/// 5. Order candidates by preference (locked version first for non-targeted
///    updates, otherwise highest version first).
/// 6. Recursively search candidates, adding transitive constraints and
///    backtracking whenever a branch becomes inconsistent.
///
/// Single-version rule: each module path appears at most once.
pub fn solve(
    root_source: &str,
    root_vo: &ToolchainConstraint,
    root_requires: &[(ModulePath, DepConstraint)],
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<ResolvedGraph, Error> {
    let mut constraints: BTreeMap<ModulePath, Vec<(String, DepConstraint)>> = BTreeMap::new();
    for (mp, constraint) in root_requires {
        constraints
            .entry(mp.clone())
            .or_default()
            .push((root_source.to_string(), constraint.clone()));
    }

    solve_search(root_vo, registry, prefs, BTreeMap::new(), constraints)
}

fn solve_search(
    root_vo: &ToolchainConstraint,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
    resolved: BTreeMap<ModulePath, ResolvedModule>,
    constraints: BTreeMap<ModulePath, Vec<(String, DepConstraint)>>,
) -> Result<ResolvedGraph, Error> {
    let Some((module, candidates)) =
        pick_next_module(root_vo, registry, prefs, &resolved, &constraints)?
    else {
        validate_resolved_graph(&resolved, &constraints)?;
        return Ok(ResolvedGraph { modules: resolved });
    };

    let mut first_error: Option<Error> = None;
    for candidate in candidates {
        let mut next_resolved = resolved.clone();
        let mut next_constraints = constraints.clone();
        let mut branch_error: Option<Error> = None;
        let source = module.as_str().to_string();

        for req in &candidate.manifest.require {
            let dep_mp = req.module.clone();
            next_constraints
                .entry(dep_mp.clone())
                .or_default()
                .push((source.clone(), req.constraint.clone()));

            let selected_version = if dep_mp == module {
                Some(&candidate.version)
            } else {
                next_resolved.get(&dep_mp).map(|resolved| &resolved.version)
            };
            if let Some(selected_version) = selected_version {
                if !req.constraint.satisfies(selected_version) {
                    branch_error = Some(Error::ConflictingConstraints {
                        module: dep_mp.as_str().to_string(),
                        detail: format!(
                            "selected {} but {} requires {}",
                            selected_version, module, req.constraint,
                        ),
                    });
                    break;
                }
            }
        }

        if let Some(error) = branch_error {
            if first_error.is_none() {
                first_error = Some(error);
            }
            continue;
        }

        next_resolved.insert(module.clone(), candidate);
        match solve_search(root_vo, registry, prefs, next_resolved, next_constraints) {
            Ok(graph) => return Ok(graph),
            Err(error) => {
                if first_error.is_none() {
                    first_error = Some(error);
                }
            }
        }
    }

    Err(first_error.unwrap_or_else(|| Error::NoSatisfyingVersion {
        module: module.as_str().to_string(),
        detail: "no candidate version leads to a consistent dependency graph".to_string(),
    }))
}

fn pick_next_module(
    root_vo: &ToolchainConstraint,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
    resolved: &BTreeMap<ModulePath, ResolvedModule>,
    constraints: &BTreeMap<ModulePath, Vec<(String, DepConstraint)>>,
) -> Result<Option<(ModulePath, Vec<ResolvedModule>)>, Error> {
    let mut best: Option<(ModulePath, Vec<ResolvedModule>)> = None;

    for (module, module_constraints) in constraints {
        if resolved.contains_key(module) {
            continue;
        }

        let candidates = candidate_modules(module, module_constraints, root_vo, registry, prefs)?;
        let should_replace = match &best {
            None => true,
            Some((best_module, best_candidates)) => {
                candidates.len() < best_candidates.len()
                    || (candidates.len() == best_candidates.len() && module < best_module)
            }
        };
        if should_replace {
            best = Some((module.clone(), candidates));
        }
    }

    Ok(best)
}

fn candidate_modules(
    mp: &ModulePath,
    constraints: &[(String, DepConstraint)],
    root_vo: &ToolchainConstraint,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<Vec<ResolvedModule>, Error> {
    let all_versions = registry.list_versions(mp)?;
    let compatible: Vec<ExactVersion> =
        crate::registry::filter_compatible_versions(mp, &all_versions);
    if compatible.is_empty() {
        return Err(Error::NoSatisfyingVersion {
            module: mp.as_str().to_string(),
            detail: "no compatible versions found in registry".to_string(),
        });
    }

    let satisfying: Vec<ExactVersion> = compatible
        .into_iter()
        .filter(|version| {
            constraints
                .iter()
                .all(|(_, constraint)| constraint.satisfies(version))
        })
        .collect();
    if satisfying.is_empty() {
        return Err(no_satisfying_version_error(mp, constraints));
    }

    let mut toolchain_mismatch: Option<String> = None;
    let mut first_error: Option<Error> = None;
    let mut candidates = Vec::new();
    for version in satisfying {
        let (manifest, manifest_raw) = match registry.fetch_manifest_raw(mp, &version) {
            Ok(result) => result,
            Err(error) => {
                if first_error.is_none() {
                    first_error = Some(error);
                }
                continue;
            }
        };
        if let Err(error) = crate::registry::validate_manifest(&manifest, mp, &version) {
            if first_error.is_none() {
                first_error = Some(error);
            }
            continue;
        }
        if !root_vo.is_subset_of(&manifest.vo) {
            if toolchain_mismatch.is_none() {
                toolchain_mismatch = Some(manifest.vo.to_string());
            }
            continue;
        }
        candidates.push(ResolvedModule {
            version,
            manifest,
            manifest_raw,
        });
    }

    if candidates.is_empty() {
        if let Some(dependency_constraint) = toolchain_mismatch {
            return Err(Error::DependencyToolchainMismatch {
                module: mp.as_str().to_string(),
                project_constraint: root_vo.to_string(),
                dependency_constraint,
            });
        }
        if let Some(error) = first_error {
            return Err(error);
        }
        return Err(no_satisfying_version_error(mp, constraints));
    }

    candidates.sort_by(|left, right| right.version.cmp(&left.version));
    if should_prefer_locked(mp, prefs) {
        if let Some(locked_version) = prefs.locked.get(mp) {
            if let Some(index) = candidates
                .iter()
                .position(|candidate| candidate.version == *locked_version)
            {
                if index != 0 {
                    let locked_candidate = candidates.remove(index);
                    candidates.insert(0, locked_candidate);
                }
            }
        }
    }

    Ok(candidates)
}

fn should_prefer_locked(mp: &ModulePath, prefs: &SolvePreferences) -> bool {
    match &prefs.target_update {
        Some(target) => *target != *mp,
        None => false,
    }
}

fn no_satisfying_version_error(mp: &ModulePath, constraints: &[(String, DepConstraint)]) -> Error {
    let detail = constraints
        .iter()
        .map(|(source, constraint)| format!("  {source} requires: {constraint}"))
        .collect::<Vec<_>>()
        .join("\n");
    Error::NoSatisfyingVersion {
        module: mp.as_str().to_string(),
        detail,
    }
}

fn validate_resolved_graph(
    resolved: &BTreeMap<ModulePath, ResolvedModule>,
    constraints: &BTreeMap<ModulePath, Vec<(String, DepConstraint)>>,
) -> Result<(), Error> {
    for (mp, module_constraints) in constraints {
        let resolved_module = resolved.get(mp).ok_or_else(|| Error::NoSatisfyingVersion {
            module: mp.as_str().to_string(),
            detail: "module was required but never resolved".to_string(),
        })?;
        for (source, constraint) in module_constraints {
            if !constraint.satisfies(&resolved_module.version) {
                return Err(Error::ConflictingConstraints {
                    module: mp.as_str().to_string(),
                    detail: format!(
                        "selected {} but {} requires {}",
                        resolved_module.version, source, constraint,
                    ),
                });
            }
        }
    }
    Ok(())
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
        let graph = solve(
            root.as_str(),
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();
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
        let graph = solve(
            root.as_str(),
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();
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
        assert!(solve(
            root.as_str(),
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default()
        )
        .is_err());
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
        let graph = solve(root.as_str(), &root_vo, &reqs, &reg, &prefs).unwrap();
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
        let result = solve(
            root.as_str(),
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        );
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("toolchain"),
            "expected toolchain error, got: {msg}"
        );
    }

    #[test]
    fn test_solve_skips_toolchain_incompatible_higher_version() {
        let mut reg = MockRegistry::new();
        reg.add_module_with_vo("github.com/acme/lib", "v1.0.0", "^1.0.0", &[]);
        reg.add_module_with_vo("github.com/acme/lib", "v1.1.0", "~2.0.0", &[]);

        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];

        let graph = solve(
            root.as_str(),
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();
        let lib = &graph.modules[&ModulePath::parse("github.com/acme/lib").unwrap()];
        assert_eq!(lib.version.to_string(), "v1.0.0");
    }

    #[test]
    fn test_solve_backtrack_diamond() {
        // Graph: root -> A v1.0.0 -> {B ^1.0.0, C ^1.0.0}
        //        B v1.1.0 -> C ~1.1.0  (>= 1.1.0, < 1.2.0)
        //        B v1.0.0 -> C ^1.0.0
        //        C: v1.0.0, v1.1.0, v1.2.0
        //
        // Greedy (LIFO queue) resolves C=v1.2.0 first (only ^1.0.0 from A),
        // then B=v1.1.0 which adds C ~1.1.0. Final validation detects
        // C=v1.2.0 violates ~1.1.0. Backtrack excludes C=v1.2.0.
        // Retry picks C=v1.1.0 which satisfies both ^1.0.0 and ~1.1.0.
        let mut reg = MockRegistry::new();
        reg.add_module(
            "github.com/acme/a",
            "v1.0.0",
            &[
                ("github.com/acme/b", "^1.0.0"),
                ("github.com/acme/c", "^1.0.0"),
            ],
        );
        reg.add_module(
            "github.com/acme/b",
            "v1.0.0",
            &[("github.com/acme/c", "^1.0.0")],
        );
        reg.add_module(
            "github.com/acme/b",
            "v1.1.0",
            &[("github.com/acme/c", "~1.1.0")],
        );
        reg.add_module("github.com/acme/c", "v1.0.0", &[]);
        reg.add_module("github.com/acme/c", "v1.1.0", &[]);
        reg.add_module("github.com/acme/c", "v1.2.0", &[]);

        let root = ModulePath::parse("github.com/acme/app").unwrap();
        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/a").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];

        let graph = solve(
            root.as_str(),
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();
        assert_eq!(graph.modules.len(), 3);

        // C should be v1.1.0 (v1.2.0 excluded by backtrack)
        let c = &graph.modules[&ModulePath::parse("github.com/acme/c").unwrap()];
        assert_eq!(c.version.to_string(), "v1.1.0");

        // B should still be v1.1.0 (highest)
        let b = &graph.modules[&ModulePath::parse("github.com/acme/b").unwrap()];
        assert_eq!(b.version.to_string(), "v1.1.0");
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
        let graph = solve(
            root.as_str(),
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();
        assert_eq!(graph.modules.len(), 1);
    }
}
