//! Deterministic dependency solving over an asynchronous registry.
//!
//! Registry reads are frozen per key for the lifetime of one solve. The
//! search order and rollback rules intentionally mirror the native solver so
//! browser and native frontends select the same graph from the same metadata.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use crate::async_install::AsyncRegistry;
use crate::identity::ModulePath;
use crate::schema::manifest::ReleaseManifest;
use crate::solver::{ResolvedGraph, ResolvedModule};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::Error;

#[derive(Debug, Clone)]
pub(crate) struct RootRequirement {
    pub module: ModulePath,
    /// `None` requests the highest compatible non-prerelease release without
    /// adding a semantic-version range. Prereleases always require an explicit
    /// constraint.
    pub constraint: Option<DepConstraint>,
}

#[derive(Debug, Clone, Copy)]
struct SolveLimits {
    modules: usize,
    edges: usize,
    artifacts: usize,
    candidates: usize,
    manifest_bytes: usize,
    decisions: usize,
    backtracks: usize,
}

impl Default for SolveLimits {
    fn default() -> Self {
        Self {
            modules: crate::MAX_MODULE_DEPENDENCIES,
            edges: crate::MAX_SOLVER_GRAPH_EDGES,
            artifacts: crate::MAX_SOLVER_GRAPH_ARTIFACTS,
            candidates: crate::MAX_SOLVER_CANDIDATES,
            manifest_bytes: crate::MAX_SOLVER_MANIFEST_BYTES,
            decisions: crate::MAX_SOLVER_SEARCH_DECISIONS,
            backtracks: crate::MAX_SOLVER_BACKTRACKS,
        }
    }
}

#[derive(Debug)]
struct FrozenManifest {
    manifest: ReleaseManifest,
    raw: Box<[u8]>,
}

#[derive(Debug, Clone)]
struct SelectedModule {
    version: ExactVersion,
    release: Arc<FrozenManifest>,
}

#[derive(Debug, Clone)]
struct FrozenError(Error);

impl FrozenError {
    fn thaw(&self) -> Error {
        self.0.clone()
    }
}

type FrozenResult<T> = Result<T, Arc<FrozenError>>;

struct RegistrySnapshot<'a, R: AsyncRegistry + ?Sized> {
    registry: &'a R,
    limits: SolveLimits,
    retained_candidates: usize,
    retained_manifest_bytes: usize,
    version_candidates: BTreeMap<ModulePath, FrozenResult<Arc<[ExactVersion]>>>,
    manifests: BTreeMap<(ModulePath, ExactVersion), FrozenResult<Arc<FrozenManifest>>>,
}

impl<'a, R: AsyncRegistry + ?Sized> RegistrySnapshot<'a, R> {
    fn new(registry: &'a R, limits: SolveLimits) -> Self {
        Self {
            registry,
            limits,
            retained_candidates: 0,
            retained_manifest_bytes: 0,
            version_candidates: BTreeMap::new(),
            manifests: BTreeMap::new(),
        }
    }

    async fn version_candidates(
        &mut self,
        module: &ModulePath,
    ) -> Result<Arc<[ExactVersion]>, Error> {
        if let Some(cached) = self.version_candidates.get(module).cloned() {
            return thaw_snapshot_result(cached);
        }

        let loaded = match self.registry.list_version_candidates(module).await {
            Ok(candidates) => crate::registry::normalize_version_candidates(module, candidates)
                .and_then(|candidates| {
                    let retained = self
                        .retained_candidates
                        .checked_add(candidates.len())
                        .ok_or_else(|| {
                            resolution_limit_error(
                                "retained registry candidate count",
                                self.limits.candidates,
                            )
                        })?;
                    if retained > self.limits.candidates {
                        return Err(resolution_limit_error(
                            "retained registry candidate count",
                            self.limits.candidates,
                        ));
                    }
                    self.retained_candidates = retained;
                    Ok(Arc::<[ExactVersion]>::from(candidates))
                }),
            Err(error) => Err(error),
        }
        .map_err(freeze_snapshot_error);

        self.version_candidates
            .insert(module.clone(), loaded.clone());
        thaw_snapshot_result(loaded)
    }

    async fn manifest(
        &mut self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Arc<FrozenManifest>, Error> {
        let key = (module.clone(), version.clone());
        if let Some(cached) = self.manifests.get(&key).cloned() {
            return thaw_snapshot_result(cached);
        }

        let loaded = match self.registry.fetch_manifest_raw(module, version).await {
            Ok(raw) => (|| {
                let retained = crate::registry::charge_processed_manifest_bytes(
                    self.retained_manifest_bytes,
                    raw.len(),
                    self.limits.manifest_bytes,
                )?;
                self.retained_manifest_bytes = retained;
                let manifest = crate::registry::parse_manifest_bytes(&raw, module, version)?;
                Ok(Arc::new(FrozenManifest {
                    manifest,
                    raw: raw.into_boxed_slice(),
                }))
            })(),
            Err(error) => Err(error),
        }
        .map_err(freeze_snapshot_error);

        self.manifests.insert(key, loaded.clone());
        thaw_snapshot_result(loaded)
    }
}

fn freeze_snapshot_error(error: Error) -> Arc<FrozenError> {
    Arc::new(FrozenError(error))
}

fn thaw_snapshot_result<T>(result: FrozenResult<T>) -> Result<T, Error> {
    result.map_err(|error| error.thaw())
}

#[derive(Debug)]
struct SearchBudget {
    limits: SolveLimits,
    decisions: usize,
    backtracks: usize,
}

impl SearchBudget {
    fn new(limits: SolveLimits) -> Self {
        Self {
            limits,
            decisions: 0,
            backtracks: 0,
        }
    }

    fn record_decision(&mut self) -> Result<(), Error> {
        increment_budget(
            &mut self.decisions,
            self.limits.decisions,
            "search decision count",
        )
    }

    fn record_backtrack(&mut self) -> Result<(), Error> {
        increment_budget(
            &mut self.backtracks,
            self.limits.backtracks,
            "backtrack count",
        )
    }
}

fn increment_budget(current: &mut usize, limit: usize, resource: &str) -> Result<(), Error> {
    let next = current
        .checked_add(1)
        .ok_or_else(|| resolution_limit_error(resource, limit))?;
    if next > limit {
        return Err(resolution_limit_error(resource, limit));
    }
    *current = next;
    Ok(())
}

fn resolution_limit_error(resource: &str, limit: usize) -> Error {
    Error::ResolutionLimitExceeded {
        resource: resource.to_string(),
        limit,
    }
}

/// Resolve one complete graph using a solve-local asynchronous registry view.
pub(crate) async fn solve<R: AsyncRegistry + ?Sized>(
    root_source: &str,
    root_vo: Option<&ToolchainConstraint>,
    root_requires: &[RootRequirement],
    registry: &R,
    preferred_versions: &BTreeMap<ModulePath, ExactVersion>,
) -> Result<ResolvedGraph, Error> {
    solve_with_limits(
        root_source,
        root_vo,
        root_requires,
        registry,
        preferred_versions,
        SolveLimits::default(),
    )
    .await
}

async fn solve_with_limits<R: AsyncRegistry + ?Sized>(
    root_source: &str,
    root_vo: Option<&ToolchainConstraint>,
    root_requires: &[RootRequirement],
    registry: &R,
    preferred_versions: &BTreeMap<ModulePath, ExactVersion>,
    limits: SolveLimits,
) -> Result<ResolvedGraph, Error> {
    if root_requires.len() > limits.edges {
        return Err(resolution_limit_error(
            "selected graph edge count",
            limits.edges,
        ));
    }

    let mut constraints: BTreeMap<ModulePath, Vec<(String, Option<DepConstraint>)>> =
        BTreeMap::new();
    for requirement in root_requires {
        constraints
            .entry(requirement.module.clone())
            .or_default()
            .push((root_source.to_string(), requirement.constraint.clone()));
    }
    if constraints.len() > limits.modules {
        return Err(resolution_limit_error(
            "selected module count",
            limits.modules,
        ));
    }

    let unresolved = constraints.keys().cloned().collect();
    let mut snapshot = RegistrySnapshot::new(registry, limits);
    let mut budget = SearchBudget::new(limits);
    let state = SearchState {
        resolved: BTreeMap::new(),
        constraints,
        unresolved,
        edge_count: root_requires.len(),
        artifact_count: 0,
    };
    solve_search(
        root_vo,
        &mut snapshot,
        &mut budget,
        preferred_versions,
        state,
    )
    .await
}

#[derive(Debug)]
struct ConstraintUndo {
    module: ModulePath,
    previous_len: Option<usize>,
}

#[derive(Debug)]
struct AppliedCandidate {
    module: ModulePath,
    previous_edge_count: usize,
    previous_artifact_count: usize,
    constraints: Vec<ConstraintUndo>,
}

#[derive(Debug)]
struct SearchFrame {
    module: ModulePath,
    candidates: Vec<SelectedModule>,
    next_candidate: usize,
    first_error: Option<Error>,
    applied: Option<AppliedCandidate>,
}

struct SearchState {
    resolved: BTreeMap<ModulePath, SelectedModule>,
    constraints: BTreeMap<ModulePath, Vec<(String, Option<DepConstraint>)>>,
    unresolved: BTreeSet<ModulePath>,
    edge_count: usize,
    artifact_count: usize,
}

async fn solve_search<R: AsyncRegistry + ?Sized>(
    root_vo: Option<&ToolchainConstraint>,
    snapshot: &mut RegistrySnapshot<'_, R>,
    budget: &mut SearchBudget,
    preferred_versions: &BTreeMap<ModulePath, ExactVersion>,
    state: SearchState,
) -> Result<ResolvedGraph, Error> {
    let SearchState {
        mut resolved,
        mut constraints,
        mut unresolved,
        mut edge_count,
        mut artifact_count,
    } = state;
    let mut frames: Vec<SearchFrame> = Vec::new();
    let mut pick_module = true;
    let mut pending_failure: Option<Error> = None;

    loop {
        if let Some(error) = pending_failure.take() {
            if should_abort_search(&error) {
                return Err(error);
            }
            let Some(frame) = frames.last_mut() else {
                return Err(error);
            };
            if let Some(applied) = frame.applied.take() {
                undo_candidate(
                    applied,
                    &mut resolved,
                    &mut constraints,
                    &mut unresolved,
                    &mut edge_count,
                    &mut artifact_count,
                );
            }
            budget.record_backtrack()?;
            if frame.first_error.is_none() {
                frame.first_error = Some(error);
            }
        }

        if pick_module {
            match pick_next_module(
                root_vo,
                snapshot,
                preferred_versions,
                &unresolved,
                &constraints,
            )
            .await
            {
                Ok(Some((module, candidates))) => {
                    frames.push(SearchFrame {
                        module,
                        candidates,
                        next_candidate: 0,
                        first_error: None,
                        applied: None,
                    });
                    pick_module = false;
                }
                Ok(None) => match validate_resolved_graph(&resolved, &constraints) {
                    Ok(()) => return Ok(materialize_resolved_graph(resolved)),
                    Err(error) => {
                        pending_failure = Some(error);
                        pick_module = false;
                        continue;
                    }
                },
                Err(error) => {
                    pending_failure = Some(error);
                    pick_module = false;
                    continue;
                }
            }
        }

        let exhausted = frames
            .last()
            .is_some_and(|frame| frame.next_candidate >= frame.candidates.len());
        if exhausted {
            let frame = frames.pop().expect("an exhausted frame exists");
            debug_assert!(frame.applied.is_none());
            pending_failure =
                Some(
                    frame
                        .first_error
                        .unwrap_or_else(|| Error::NoSatisfyingVersion {
                            module: frame.module.as_str().to_string(),
                            detail: "no candidate version leads to a consistent dependency graph"
                                .to_string(),
                        }),
                );
            continue;
        }

        let (module, candidate) = {
            let frame = frames.last_mut().expect("a search frame exists");
            debug_assert!(frame.applied.is_none());
            let candidate = frame.candidates[frame.next_candidate].clone();
            frame.next_candidate += 1;
            (frame.module.clone(), candidate)
        };
        budget.record_decision()?;

        let next_edge_count = edge_count
            .checked_add(candidate.release.manifest.require.len())
            .filter(|count| *count <= snapshot.limits.edges);
        let mut branch_error = next_edge_count
            .is_none()
            .then(|| resolution_limit_error("selected graph edge count", snapshot.limits.edges));
        let next_artifact_count = artifact_count
            .checked_add(candidate.release.manifest.artifacts.len())
            .filter(|count| *count <= snapshot.limits.artifacts);
        if branch_error.is_none() && next_artifact_count.is_none() {
            branch_error = Some(resolution_limit_error(
                "selected graph artifact count",
                snapshot.limits.artifacts,
            ));
        }

        let source = module.as_str().to_string();
        let mut new_modules = 0usize;
        if branch_error.is_none() {
            for requirement in &candidate.release.manifest.require {
                let dependency = &requirement.module;
                if !constraints.contains_key(dependency) {
                    new_modules = match new_modules.checked_add(1) {
                        Some(count) => count,
                        None => {
                            branch_error = Some(resolution_limit_error(
                                "selected module count",
                                snapshot.limits.modules,
                            ));
                            break;
                        }
                    };
                }
                let selected_version = if dependency == &module {
                    Some(&candidate.version)
                } else {
                    resolved.get(dependency).map(|selected| &selected.version)
                };
                if selected_version
                    .is_some_and(|version| !requirement.constraint.satisfies(version))
                {
                    branch_error = Some(Error::ConflictingConstraints {
                        module: dependency.as_str().to_string(),
                        detail: format!(
                            "selected {} but {} requires {}",
                            selected_version.expect("checked above"),
                            module,
                            requirement.constraint,
                        ),
                    });
                    break;
                }
            }
        }
        if branch_error.is_none()
            && constraints
                .len()
                .checked_add(new_modules)
                .is_none_or(|count| count > snapshot.limits.modules)
        {
            branch_error = Some(resolution_limit_error(
                "selected module count",
                snapshot.limits.modules,
            ));
        }
        if let Some(error) = branch_error {
            budget.record_backtrack()?;
            let frame = frames.last_mut().expect("a search frame exists");
            if frame.first_error.is_none() {
                frame.first_error = Some(error);
            }
            continue;
        }

        let mut constraint_undo = Vec::new();
        constraint_undo
            .try_reserve(candidate.release.manifest.require.len())
            .map_err(|_| {
                resolution_limit_error("search rollback state allocation", snapshot.limits.edges)
            })?;
        for requirement in &candidate.release.manifest.require {
            let dependency = requirement.module.clone();
            let previous_len = constraints.get(&dependency).map(Vec::len);
            constraints
                .entry(dependency.clone())
                .or_default()
                .push((source.clone(), Some(requirement.constraint.clone())));
            if previous_len.is_none() {
                unresolved.insert(dependency.clone());
            }
            constraint_undo.push(ConstraintUndo {
                module: dependency,
                previous_len,
            });
        }
        let previous_edge_count = edge_count;
        let previous_artifact_count = artifact_count;
        edge_count = next_edge_count.expect("validated edge count");
        artifact_count = next_artifact_count.expect("validated artifact count");
        assert!(resolved.insert(module.clone(), candidate).is_none());
        assert!(unresolved.remove(&module));
        frames.last_mut().expect("a search frame exists").applied = Some(AppliedCandidate {
            module,
            previous_edge_count,
            previous_artifact_count,
            constraints: constraint_undo,
        });
        pick_module = true;
    }
}

fn undo_candidate(
    applied: AppliedCandidate,
    resolved: &mut BTreeMap<ModulePath, SelectedModule>,
    constraints: &mut BTreeMap<ModulePath, Vec<(String, Option<DepConstraint>)>>,
    unresolved: &mut BTreeSet<ModulePath>,
    edge_count: &mut usize,
    artifact_count: &mut usize,
) {
    assert!(resolved.remove(&applied.module).is_some());
    unresolved.insert(applied.module);
    for undo in applied.constraints.into_iter().rev() {
        match undo.previous_len {
            Some(previous_len) => constraints
                .get_mut(&undo.module)
                .expect("existing constraint entry")
                .truncate(previous_len),
            None => {
                assert!(constraints.remove(&undo.module).is_some());
                unresolved.remove(&undo.module);
            }
        }
    }
    *edge_count = applied.previous_edge_count;
    *artifact_count = applied.previous_artifact_count;
}

fn should_abort_search(error: &Error) -> bool {
    match error {
        Error::NoSatisfyingVersion { .. }
        | Error::ConflictingConstraints { .. }
        | Error::DependencyToolchainMismatch { .. }
        | Error::InvalidReleaseMetadata(_)
        | Error::ManifestParse(_) => false,
        Error::ResolutionLimitExceeded { resource, .. } => {
            resource != "selected graph edge count"
                && resource != "selected graph artifact count"
                && resource != "selected module count"
        }
        _ => true,
    }
}

async fn pick_next_module<R: AsyncRegistry + ?Sized>(
    root_vo: Option<&ToolchainConstraint>,
    snapshot: &mut RegistrySnapshot<'_, R>,
    preferred_versions: &BTreeMap<ModulePath, ExactVersion>,
    unresolved: &BTreeSet<ModulePath>,
    constraints: &BTreeMap<ModulePath, Vec<(String, Option<DepConstraint>)>>,
) -> Result<Option<(ModulePath, Vec<SelectedModule>)>, Error> {
    let Some(module) = unresolved.first().cloned() else {
        return Ok(None);
    };
    let module_constraints = constraints
        .get(&module)
        .expect("every unresolved module has constraints")
        .clone();
    let candidates = candidate_modules(
        &module,
        &module_constraints,
        root_vo,
        snapshot,
        preferred_versions,
    )
    .await?;
    Ok(Some((module, candidates)))
}

async fn candidate_modules<R: AsyncRegistry + ?Sized>(
    module: &ModulePath,
    constraints: &[(String, Option<DepConstraint>)],
    root_vo: Option<&ToolchainConstraint>,
    snapshot: &mut RegistrySnapshot<'_, R>,
    preferred_versions: &BTreeMap<ModulePath, ExactVersion>,
) -> Result<Vec<SelectedModule>, Error> {
    let all_versions = snapshot.version_candidates(module).await?;
    let compatible = crate::registry::filter_compatible_versions(module, all_versions.as_ref());
    if compatible.is_empty() {
        return Err(Error::NoSatisfyingVersion {
            module: module.as_str().to_string(),
            detail: "no compatible versions found in registry".to_string(),
        });
    }

    let satisfying = compatible.into_iter().filter(|version| {
        constraints.iter().all(|(_, constraint)| {
            match constraint {
                Some(constraint) => constraint.satisfies(version),
                // An omitted direct constraint means the latest stable
                // release. Prereleases require an explicit prerelease range.
                None => !version.semver().is_prerelease(),
            }
        })
    });

    let mut found_satisfying_version = false;
    let mut toolchain_mismatch: Option<String> = None;
    let mut first_invalid_release: Option<Error> = None;
    let mut candidates = Vec::new();
    for version in satisfying {
        found_satisfying_version = true;
        let release = match snapshot.manifest(module, &version).await {
            Ok(release) => release,
            Err(error) if crate::registry::is_definitive_invalid_release_error(&error) => {
                if first_invalid_release.is_none() {
                    first_invalid_release = Some(error);
                }
                continue;
            }
            Err(error) => return Err(error),
        };
        if let Some(root_vo) = root_vo {
            if !root_vo.is_subset_of(&release.manifest.vo) {
                if toolchain_mismatch.is_none() {
                    toolchain_mismatch = Some(release.manifest.vo.to_string());
                }
                continue;
            }
        }
        candidates.push(SelectedModule { version, release });
    }

    if candidates.is_empty() {
        if !found_satisfying_version {
            return Err(no_satisfying_version_error(module, constraints));
        }
        if let Some(error) = first_invalid_release {
            return Err(error);
        }
        if let (Some(project_constraint), Some(dependency_constraint)) =
            (root_vo, toolchain_mismatch)
        {
            return Err(Error::DependencyToolchainMismatch {
                module: module.as_str().to_string(),
                project_constraint: project_constraint.to_string(),
                dependency_constraint,
            });
        }
        return Err(no_satisfying_version_error(module, constraints));
    }

    candidates.sort_by(|left, right| right.version.cmp(&left.version));
    if let Some(preferred) = preferred_versions.get(module) {
        if let Some(index) = candidates
            .iter()
            .position(|candidate| candidate.version == *preferred)
        {
            if index != 0 {
                let preferred = candidates.remove(index);
                candidates.insert(0, preferred);
            }
        }
    }
    Ok(candidates)
}

fn no_satisfying_version_error(
    module: &ModulePath,
    constraints: &[(String, Option<DepConstraint>)],
) -> Error {
    let detail = constraints
        .iter()
        .map(|(source, constraint)| match constraint {
            Some(constraint) => format!("  {source} requires: {constraint}"),
            None => format!("  {source} requests the latest compatible release"),
        })
        .collect::<Vec<_>>()
        .join("\n");
    Error::NoSatisfyingVersion {
        module: module.as_str().to_string(),
        detail,
    }
}

fn validate_resolved_graph(
    resolved: &BTreeMap<ModulePath, SelectedModule>,
    constraints: &BTreeMap<ModulePath, Vec<(String, Option<DepConstraint>)>>,
) -> Result<(), Error> {
    for (module, module_constraints) in constraints {
        let resolved_module = resolved
            .get(module)
            .ok_or_else(|| Error::NoSatisfyingVersion {
                module: module.as_str().to_string(),
                detail: "module was required but never resolved".to_string(),
            })?;
        for (source, constraint) in module_constraints {
            if let Some(constraint) = constraint {
                if !constraint.satisfies(&resolved_module.version) {
                    return Err(Error::ConflictingConstraints {
                        module: module.as_str().to_string(),
                        detail: format!(
                            "selected {} but {} requires {}",
                            resolved_module.version, source, constraint,
                        ),
                    });
                }
            }
        }
    }
    Ok(())
}

fn materialize_resolved_graph(resolved: BTreeMap<ModulePath, SelectedModule>) -> ResolvedGraph {
    let modules = resolved
        .into_iter()
        .map(|(module, selected)| {
            (
                module,
                ResolvedModule {
                    version: selected.version,
                    manifest: selected.release.manifest.clone(),
                    manifest_raw: selected.release.raw.to_vec(),
                },
            )
        })
        .collect();
    ResolvedGraph { modules }
}

#[cfg(test)]
mod tests;
