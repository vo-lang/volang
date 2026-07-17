use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

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
    /// Used to compute the `release` digest for `vo.lock` and
    /// to write the manifest file into the module cache.
    pub manifest_raw: Vec<u8>,
}

/// How a solve should treat versions selected by an existing lock file.
#[derive(Debug, Clone, Default)]
enum LockedVersionPolicy {
    /// Ignore prior selections and choose the highest valid solution.
    #[default]
    UpdateAll,
    /// Prefer every prior selection that remains valid.
    PreserveLocked,
    /// Update one module while preserving every unrelated valid selection.
    UpdateTarget(ModulePath),
}

/// Explicit version-selection preferences for one dependency solve.
///
/// The default performs a full update. Mutation commands that need stable
/// unrelated selections must opt into [`Self::preserve_locked`] or
/// [`Self::update_target`].
#[derive(Debug, Clone, Default)]
pub struct SolvePreferences {
    policy: LockedVersionPolicy,
    locked: BTreeMap<ModulePath, ExactVersion>,
}

impl SolvePreferences {
    /// Prefer every supplied locked version while it satisfies the new graph.
    pub fn preserve_locked(locked: BTreeMap<ModulePath, ExactVersion>) -> Self {
        Self {
            policy: LockedVersionPolicy::PreserveLocked,
            locked,
        }
    }

    /// Update `target` and prefer supplied locked versions for every other module.
    pub fn update_target(target: ModulePath, locked: BTreeMap<ModulePath, ExactVersion>) -> Self {
        Self {
            policy: LockedVersionPolicy::UpdateTarget(target),
            locked,
        }
    }
}

#[derive(Debug)]
struct FrozenManifest {
    manifest: ReleaseManifest,
    raw: Box<[u8]>,
}

type FrozenManifestResults =
    BTreeMap<(ModulePath, ExactVersion), FrozenResult<Arc<FrozenManifest>>>;

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
struct SearchBudget {
    limits: SolveLimits,
    decisions: std::cell::Cell<usize>,
    backtracks: std::cell::Cell<usize>,
}

impl SearchBudget {
    fn new(limits: SolveLimits) -> Self {
        Self {
            limits,
            decisions: std::cell::Cell::new(0),
            backtracks: std::cell::Cell::new(0),
        }
    }

    fn record_decision(&self) -> Result<(), Error> {
        increment_budget(
            &self.decisions,
            self.limits.decisions,
            "search decision count",
        )
    }

    fn record_backtrack(&self) -> Result<(), Error> {
        increment_budget(&self.backtracks, self.limits.backtracks, "backtrack count")
    }
}

fn increment_budget(
    current: &std::cell::Cell<usize>,
    limit: usize,
    resource: &str,
) -> Result<(), Error> {
    let next = current
        .get()
        .checked_add(1)
        .ok_or_else(|| resolution_limit_error(resource, limit))?;
    if next > limit {
        return Err(resolution_limit_error(resource, limit));
    }
    current.set(next);
    Ok(())
}

fn resolution_limit_error(resource: &str, limit: usize) -> Error {
    Error::ResolutionLimitExceeded {
        resource: resource.to_string(),
        limit,
    }
}

/// A solve-local, key-stable view of registry metadata.
///
/// Entries are loaded lazily because the transitive module set is only known
/// after manifests are inspected. The first result for each key, including an
/// error, is retained for the entire solve. Recursive search therefore never
/// observes later registry changes and never repeats a registry request.
struct SolverRegistrySnapshot<'a> {
    registry: &'a dyn Registry,
    limits: SolveLimits,
    retained_candidates: std::cell::Cell<usize>,
    retained_manifest_bytes: std::cell::Cell<usize>,
    version_candidates: RefCell<BTreeMap<ModulePath, FrozenResult<Arc<[ExactVersion]>>>>,
    manifests: RefCell<FrozenManifestResults>,
}

impl<'a> SolverRegistrySnapshot<'a> {
    fn new(registry: &'a dyn Registry, limits: SolveLimits) -> Self {
        Self {
            registry,
            limits,
            retained_candidates: std::cell::Cell::new(0),
            retained_manifest_bytes: std::cell::Cell::new(0),
            version_candidates: RefCell::new(BTreeMap::new()),
            manifests: RefCell::new(BTreeMap::new()),
        }
    }

    fn version_candidates(&self, module: &ModulePath) -> Result<Arc<[ExactVersion]>, Error> {
        if let Some(cached) = self.version_candidates.borrow().get(module).cloned() {
            return thaw_snapshot_result(cached);
        }

        let loaded = self
            .registry
            .list_version_candidates(module)
            .and_then(|candidates| {
                crate::registry::normalize_version_candidates(module, candidates)
            })
            .and_then(|candidates| {
                let retained = self
                    .retained_candidates
                    .get()
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
                self.retained_candidates.set(retained);
                Ok(Arc::<[ExactVersion]>::from(candidates))
            })
            .map_err(freeze_snapshot_error);
        self.version_candidates
            .borrow_mut()
            .insert(module.clone(), loaded.clone());
        thaw_snapshot_result(loaded)
    }

    fn manifest(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Arc<FrozenManifest>, Error> {
        let key = (module.clone(), version.clone());
        if let Some(cached) = self.manifests.borrow().get(&key).cloned() {
            return thaw_snapshot_result(cached);
        }

        let loaded = self
            .registry
            .fetch_manifest_raw(module, version)
            .and_then(|raw| {
                let retained = crate::registry::charge_processed_manifest_bytes(
                    self.retained_manifest_bytes.get(),
                    raw.len(),
                    self.limits.manifest_bytes,
                )?;
                self.retained_manifest_bytes.set(retained);
                let manifest = crate::registry::parse_manifest_bytes(&raw, module, version)?;
                Ok(Arc::new(FrozenManifest {
                    manifest,
                    raw: raw.into_boxed_slice(),
                }))
            })
            .map_err(freeze_snapshot_error);
        self.manifests.borrow_mut().insert(key, loaded.clone());
        thaw_snapshot_result(loaded)
    }
}

fn freeze_snapshot_error(error: Error) -> Arc<FrozenError> {
    Arc::new(FrozenError(error))
}

fn thaw_snapshot_result<T>(result: FrozenResult<T>) -> Result<T, Error> {
    result.map_err(|error| error.thaw())
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
/// 1. Start with root module's direct dependencies.
/// 2. For each unresolved module, fetch candidate versions from registry.
/// 3. Filter to compatible versions (major-path rule + constraint satisfaction).
/// 4. Filter out candidates whose release manifest toolchain constraint does not
///    support the root project's toolchain constraint.
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
    solve_with_limits(
        root_source,
        root_vo,
        root_requires,
        registry,
        prefs,
        SolveLimits::default(),
    )
}

fn solve_with_limits(
    root_source: &str,
    root_vo: &ToolchainConstraint,
    root_requires: &[(ModulePath, DepConstraint)],
    registry: &dyn Registry,
    prefs: &SolvePreferences,
    limits: SolveLimits,
) -> Result<ResolvedGraph, Error> {
    if root_requires.len() > limits.edges {
        return Err(resolution_limit_error(
            "selected graph edge count",
            limits.edges,
        ));
    }
    let mut constraints: BTreeMap<ModulePath, Vec<(String, DepConstraint)>> = BTreeMap::new();
    for (mp, constraint) in root_requires {
        constraints
            .entry(mp.clone())
            .or_default()
            .push((root_source.to_string(), constraint.clone()));
    }
    if constraints.len() > limits.modules {
        return Err(resolution_limit_error(
            "selected module count",
            limits.modules,
        ));
    }

    let snapshot = SolverRegistrySnapshot::new(registry, limits);
    let budget = SearchBudget::new(limits);
    let unresolved = constraints.keys().cloned().collect();
    let state = SearchState {
        resolved: BTreeMap::new(),
        constraints,
        unresolved,
        edge_count: root_requires.len(),
        artifact_count: 0,
    };
    solve_search(root_vo, &snapshot, &budget, prefs, state)
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
    constraints: BTreeMap<ModulePath, Vec<(String, DepConstraint)>>,
    unresolved: BTreeSet<ModulePath>,
    edge_count: usize,
    artifact_count: usize,
}

fn solve_search(
    root_vo: &ToolchainConstraint,
    snapshot: &SolverRegistrySnapshot<'_>,
    budget: &SearchBudget,
    prefs: &SolvePreferences,
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
            match pick_next_module(root_vo, snapshot, prefs, &unresolved, &constraints) {
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
            .checked_add(candidate.release.manifest.dependencies.len())
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
            for release_dependency in &candidate.release.manifest.dependencies {
                let dependency = &release_dependency.module;
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
                    .is_some_and(|version| !release_dependency.constraint.satisfies(version))
                {
                    branch_error = Some(Error::ConflictingConstraints {
                        module: dependency.as_str().to_string(),
                        detail: format!(
                            "selected {} but {} requires {}",
                            selected_version.expect("checked above"),
                            module,
                            release_dependency.constraint,
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
            .try_reserve(candidate.release.manifest.dependencies.len())
            .map_err(|_| {
                resolution_limit_error("search rollback state allocation", snapshot.limits.edges)
            })?;
        for release_dependency in &candidate.release.manifest.dependencies {
            let dependency = release_dependency.module.clone();
            let previous_len = constraints.get(&dependency).map(Vec::len);
            constraints
                .entry(dependency.clone())
                .or_default()
                .push((source.clone(), release_dependency.constraint.clone()));
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
    constraints: &mut BTreeMap<ModulePath, Vec<(String, DepConstraint)>>,
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

fn pick_next_module(
    root_vo: &ToolchainConstraint,
    snapshot: &SolverRegistrySnapshot<'_>,
    prefs: &SolvePreferences,
    unresolved: &BTreeSet<ModulePath>,
    constraints: &BTreeMap<ModulePath, Vec<(String, DepConstraint)>>,
) -> Result<Option<(ModulePath, Vec<SelectedModule>)>, Error> {
    let Some(module) = unresolved.first() else {
        return Ok(None);
    };
    let module_constraints = constraints
        .get(module)
        .expect("every unresolved module has constraints");
    let candidates = candidate_modules(module, module_constraints, root_vo, snapshot, prefs)?;
    Ok(Some((module.clone(), candidates)))
}

fn candidate_modules(
    mp: &ModulePath,
    constraints: &[(String, DepConstraint)],
    root_vo: &ToolchainConstraint,
    snapshot: &SolverRegistrySnapshot<'_>,
    prefs: &SolvePreferences,
) -> Result<Vec<SelectedModule>, Error> {
    let all_versions = snapshot.version_candidates(mp)?;
    let compatible: Vec<ExactVersion> =
        crate::registry::filter_compatible_versions(mp, all_versions.as_ref());
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
        let release = match snapshot.manifest(mp, &version) {
            Ok(result) => result,
            Err(error) if crate::registry::is_definitive_invalid_release_error(&error) => {
                if first_error.is_none() {
                    first_error = Some(error);
                }
                continue;
            }
            Err(error) => return Err(error),
        };
        if !root_vo.is_subset_of(&release.manifest.vo) {
            if toolchain_mismatch.is_none() {
                toolchain_mismatch = Some(release.manifest.vo.to_string());
            }
            continue;
        }
        candidates.push(SelectedModule { version, release });
    }

    if candidates.is_empty() {
        // Any failed registry/manifest read leaves the candidate set
        // indeterminate.  Report that frozen error before claiming every
        // release is definitively toolchain-incompatible.
        if let Some(error) = first_error {
            return Err(error);
        }
        if let Some(dependency_constraint) = toolchain_mismatch {
            return Err(Error::DependencyToolchainMismatch {
                module: mp.as_str().to_string(),
                project_constraint: root_vo.to_string(),
                dependency_constraint,
            });
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
    match &prefs.policy {
        LockedVersionPolicy::UpdateAll => false,
        LockedVersionPolicy::PreserveLocked => true,
        LockedVersionPolicy::UpdateTarget(target) => *target != *mp,
    }
}

fn no_satisfying_version_error(mp: &ModulePath, constraints: &[(String, DepConstraint)]) -> Error {
    const MAX_RENDERED_CONSTRAINTS: usize = 8;
    use std::fmt::Write as _;

    let mut detail = String::new();
    for (index, (source, constraint)) in constraints
        .iter()
        .take(MAX_RENDERED_CONSTRAINTS)
        .enumerate()
    {
        if index != 0 {
            detail.push('\n');
        }
        let _ = write!(detail, "  {source} requires: {constraint}");
    }
    let omitted = constraints.len().saturating_sub(MAX_RENDERED_CONSTRAINTS);
    if omitted != 0 {
        if !detail.is_empty() {
            detail.push('\n');
        }
        let _ = write!(detail, "  ... and {omitted} more constraints");
    }
    Error::NoSatisfyingVersion {
        module: mp.as_str().to_string(),
        detail,
    }
}

fn validate_resolved_graph(
    resolved: &BTreeMap<ModulePath, SelectedModule>,
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
mod tests {
    use super::*;
    use crate::schema::manifest::{
        ManifestArtifact, ManifestDependency, ManifestPackage, ManifestSource, ReleaseManifest,
    };
    use std::collections::HashMap;
    use std::sync::Mutex;

    struct MockRegistry {
        versions: HashMap<String, Vec<ExactVersion>>,
        manifests: HashMap<(String, String), ReleaseManifest>,
        manifest_raw_overrides: HashMap<(String, String), Vec<u8>>,
        list_calls: Mutex<HashMap<String, usize>>,
        manifest_calls: Mutex<HashMap<(String, String), usize>>,
        list_network_failures: HashMap<String, String>,
        manifest_network_failures: HashMap<(String, String), String>,
        reject_repeat_reads: bool,
    }

    impl MockRegistry {
        fn new() -> Self {
            Self {
                versions: HashMap::new(),
                manifests: HashMap::new(),
                manifest_raw_overrides: HashMap::new(),
                list_calls: Mutex::new(HashMap::new()),
                manifest_calls: Mutex::new(HashMap::new()),
                list_network_failures: HashMap::new(),
                manifest_network_failures: HashMap::new(),
                reject_repeat_reads: false,
            }
        }

        fn set_manifest_raw(&mut self, path: &str, version: &str, raw: &[u8]) {
            self.manifest_raw_overrides
                .insert((path.to_string(), version.to_string()), raw.to_vec());
        }

        fn reject_repeat_reads(&mut self) {
            self.reject_repeat_reads = true;
        }

        fn fail_version_list_with_network(&mut self, path: &str, message: &str) {
            self.list_network_failures
                .insert(path.to_string(), message.to_string());
        }

        fn fail_manifest_with_network(&mut self, path: &str, version: &str, message: &str) {
            self.manifest_network_failures
                .insert((path.to_string(), version.to_string()), message.to_string());
        }

        fn list_call_count(&self, path: &str) -> usize {
            self.list_calls
                .lock()
                .unwrap()
                .get(path)
                .copied()
                .unwrap_or(0)
        }

        fn manifest_call_count(&self, path: &str, version: &str) -> usize {
            self.manifest_calls
                .lock()
                .unwrap()
                .get(&(path.to_string(), version.to_string()))
                .copied()
                .unwrap_or(0)
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

            let mut dependencies: Vec<ManifestDependency> = deps
                .iter()
                .map(|(m, c)| ManifestDependency {
                    module: ModulePath::parse(m).unwrap(),
                    constraint: DepConstraint::parse(c).unwrap(),
                })
                .collect();
            dependencies.sort_by(|left, right| left.module.cmp(&right.module));

            let manifest = ReleaseManifest {
                schema_version: 2,
                module: mp.clone(),
                version: ev.clone(),
                commit: "a".repeat(40),
                vo: crate::version::ToolchainConstraint::parse(vo).unwrap(),
                dependencies,
                source: ManifestSource {
                    name: "source.tar.gz".into(),
                    size: 100,
                    digest: crate::digest::Digest::parse(
                        "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                    )
                    .unwrap(),
                },
                package: ManifestPackage {
                    size: 3,
                    digest: crate::digest::Digest::from_sha256(b"{}\n"),
                },
                artifacts: vec![],
            };
            self.manifests
                .insert((path.to_string(), ver.to_string()), manifest);
        }

        fn add_wasm_artifact(&mut self, path: &str, version: &str) {
            self.manifests
                .get_mut(&(path.to_string(), version.to_string()))
                .unwrap()
                .artifacts
                .push(ManifestArtifact {
                    id: crate::identity::ArtifactId {
                        kind: "extension-wasm".to_string(),
                        target: "wasm32-unknown-unknown".to_string(),
                        name: "extension.wasm".to_string(),
                    },
                    size: 1,
                    digest: crate::digest::Digest::from_sha256(b"artifact"),
                });
        }
    }

    impl Registry for MockRegistry {
        fn list_version_candidates(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
            let call_count = {
                let mut calls = self.list_calls.lock().unwrap();
                let count = calls.entry(module.as_str().to_string()).or_default();
                *count += 1;
                *count
            };
            if self.reject_repeat_reads && call_count > 1 {
                return Err(Error::RegistryError(format!(
                    "registry version index changed after first read for {module}"
                )));
            }
            if let Some(message) = self.list_network_failures.get(module.as_str()) {
                return Err(Error::Network(message.clone()));
            }
            Ok(self
                .versions
                .get(module.as_str())
                .cloned()
                .unwrap_or_default())
        }

        fn fetch_manifest_raw(
            &self,
            module: &ModulePath,
            version: &ExactVersion,
        ) -> Result<Vec<u8>, Error> {
            let key = (module.as_str().to_string(), version.to_string());
            let call_count = {
                let mut calls = self.manifest_calls.lock().unwrap();
                let count = calls.entry(key.clone()).or_default();
                *count += 1;
                *count
            };
            if self.reject_repeat_reads && call_count > 1 {
                return Err(Error::RegistryError(format!(
                    "registry manifest changed after first read for {module} {version}"
                )));
            }
            if let Some(message) = self.manifest_network_failures.get(&key) {
                return Err(Error::Network(message.clone()));
            }
            if let Some(raw) = self.manifest_raw_overrides.get(&key) {
                return Ok(raw.clone());
            }
            let manifest = self.manifests.get(&key).cloned().ok_or_else(|| {
                Error::RegistryError(format!("no manifest for {} {}", module, version))
            })?;
            Ok(manifest.render()?.into_bytes())
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
            _artifact: &crate::identity::ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("not implemented in mock".into()))
        }
    }

    #[test]
    fn test_solve_simple() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/lib", "1.2.0", &[]);
        reg.add_module("github.com/acme/lib", "1.3.0", &[]);

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
        assert_eq!(lib.version.to_string(), "1.3.0"); // highest
    }

    #[test]
    fn test_solve_transitive() {
        let mut reg = MockRegistry::new();
        reg.add_module(
            "github.com/acme/lib",
            "1.0.0",
            &[("github.com/acme/util", "^1.0.0")],
        );
        reg.add_module("github.com/acme/util", "1.0.0", &[]);
        reg.add_module("github.com/acme/util", "1.1.0", &[]);

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
        assert_eq!(util.version.to_string(), "1.1.0"); // highest
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
        reg.add_module("github.com/acme/lib", "1.0.0", &[]);
        reg.add_module("github.com/acme/lib", "1.1.0", &[]);
        reg.add_module("github.com/acme/other", "1.0.0", &[]);
        reg.add_module("github.com/acme/other", "1.2.0", &[]);

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
            ExactVersion::parse("1.0.0").unwrap(),
        );
        locked.insert(
            ModulePath::parse("github.com/acme/other").unwrap(),
            ExactVersion::parse("1.0.0").unwrap(),
        );

        let prefs = SolvePreferences::update_target(
            ModulePath::parse("github.com/acme/other").unwrap(),
            locked,
        );

        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let graph = solve(root.as_str(), &root_vo, &reqs, &reg, &prefs).unwrap();
        // lib should stay at locked 1.0.0
        let lib = &graph.modules[&ModulePath::parse("github.com/acme/lib").unwrap()];
        assert_eq!(lib.version.to_string(), "1.0.0");
        // other should get highest (it's the target)
        let other = &graph.modules[&ModulePath::parse("github.com/acme/other").unwrap()];
        assert_eq!(other.version.to_string(), "1.2.0");
    }

    #[test]
    fn preserve_locked_policy_keeps_every_still_valid_selection() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/lib", "1.0.0", &[]);
        reg.add_module("github.com/acme/lib", "1.1.0", &[]);
        reg.add_module("github.com/acme/other", "1.0.0", &[]);
        reg.add_module("github.com/acme/other", "1.2.0", &[]);

        let lib = ModulePath::parse("github.com/acme/lib").unwrap();
        let other = ModulePath::parse("github.com/acme/other").unwrap();
        let requirements = vec![
            (lib.clone(), DepConstraint::parse("^1.0.0").unwrap()),
            (other.clone(), DepConstraint::parse("^1.0.0").unwrap()),
        ];
        let locked = BTreeMap::from([
            (lib.clone(), ExactVersion::parse("1.0.0").unwrap()),
            (other.clone(), ExactVersion::parse("1.0.0").unwrap()),
        ]);

        let graph = solve(
            "github.com/acme/app",
            &ToolchainConstraint::parse("^1.0.0").unwrap(),
            &requirements,
            &reg,
            &SolvePreferences::preserve_locked(locked),
        )
        .unwrap();

        assert_eq!(graph.modules[&lib].version.to_string(), "1.0.0");
        assert_eq!(graph.modules[&other].version.to_string(), "1.0.0");
    }

    #[test]
    fn test_solve_toolchain_mismatch() {
        let mut reg = MockRegistry::new();
        // Dependency requires ~2.0.0 but root requires ^1.0.0 — root is not subset of ~2.0.0.
        reg.add_module_with_vo("github.com/acme/lib", "1.0.0", "~2.0.0", &[]);

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
    fn no_satisfying_version_diagnostic_has_a_fixed_constraint_budget() {
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let constraints = (0..20)
            .map(|index| {
                (
                    format!("source-{index}"),
                    DepConstraint::parse("^1.0.0").unwrap(),
                )
            })
            .collect::<Vec<_>>();

        let Error::NoSatisfyingVersion { detail, .. } =
            no_satisfying_version_error(&module, &constraints)
        else {
            panic!("expected no-satisfying-version error");
        };

        assert!(detail.contains("source-7"), "{detail}");
        assert!(!detail.contains("source-8"), "{detail}");
        assert!(detail.contains("and 12 more constraints"), "{detail}");
    }

    #[test]
    fn manifest_read_error_precedes_partial_toolchain_mismatch() {
        let mut reg = MockRegistry::new();
        reg.add_module_with_vo("github.com/acme/lib", "1.0.0", "~2.0.0", &[]);
        reg.add_module_with_vo("github.com/acme/lib", "1.1.0", "~2.0.0", &[]);
        reg.fail_manifest_with_network(
            "github.com/acme/lib",
            "1.1.0",
            "newest manifest unavailable exactly",
        );

        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];
        let error = solve(
            "github.com/acme/app",
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap_err();
        assert!(
            matches!(&error, Error::Network(message) if message == "newest manifest unavailable exactly")
        );
        assert_eq!(reg.manifest_call_count("github.com/acme/lib", "1.1.0"), 1);
        assert_eq!(reg.manifest_call_count("github.com/acme/lib", "1.0.0"), 0);
    }

    #[test]
    fn test_solve_skips_toolchain_incompatible_higher_version() {
        let mut reg = MockRegistry::new();
        reg.add_module_with_vo("github.com/acme/lib", "1.0.0", "^1.0.0", &[]);
        reg.add_module_with_vo("github.com/acme/lib", "1.1.0", "~2.0.0", &[]);

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
        assert_eq!(lib.version.to_string(), "1.0.0");
    }

    #[test]
    fn test_solve_backtrack_diamond() {
        // Graph: root -> A 1.0.0 -> {B ^1.0.0, C ^1.0.0}
        //        B 1.1.0 -> C ~1.1.0  (>= 1.1.0, < 1.2.0)
        //        B 1.0.0 -> C ^1.0.0
        //        C: 1.0.0, 1.1.0, 1.2.0
        //
        // Greedy (LIFO queue) resolves C=1.2.0 first (only ^1.0.0 from A),
        // then B=1.1.0 which adds C ~1.1.0. Final validation detects
        // C=1.2.0 violates ~1.1.0. Backtrack excludes C=1.2.0.
        // Retry picks C=1.1.0 which satisfies both ^1.0.0 and ~1.1.0.
        let mut reg = MockRegistry::new();
        reg.add_module(
            "github.com/acme/a",
            "1.0.0",
            &[
                ("github.com/acme/b", "^1.0.0"),
                ("github.com/acme/c", "^1.0.0"),
            ],
        );
        reg.add_module(
            "github.com/acme/b",
            "1.0.0",
            &[("github.com/acme/c", "^1.0.0")],
        );
        reg.add_module(
            "github.com/acme/b",
            "1.1.0",
            &[("github.com/acme/c", "~1.1.0")],
        );
        reg.add_module("github.com/acme/c", "1.0.0", &[]);
        reg.add_module("github.com/acme/c", "1.1.0", &[]);
        reg.add_module("github.com/acme/c", "1.2.0", &[]);

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

        // C should be 1.1.0 (1.2.0 excluded by backtrack)
        let c = &graph.modules[&ModulePath::parse("github.com/acme/c").unwrap()];
        assert_eq!(c.version.to_string(), "1.1.0");

        // B should still be 1.1.0 (highest)
        let b = &graph.modules[&ModulePath::parse("github.com/acme/b").unwrap()];
        assert_eq!(b.version.to_string(), "1.1.0");
    }

    #[test]
    fn module_lexicographic_order_defines_the_global_solution() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/a", "1.0.0", &[]);
        reg.add_module(
            "github.com/acme/a",
            "1.1.0",
            &[("github.com/acme/b", "1.0.0")],
        );
        reg.add_module("github.com/acme/b", "1.0.0", &[]);
        reg.add_module(
            "github.com/acme/b",
            "1.1.0",
            &[("github.com/acme/a", "1.0.0")],
        );
        let dependencies = vec![
            (
                ModulePath::parse("github.com/acme/b").unwrap(),
                DepConstraint::parse("^1.0.0").unwrap(),
            ),
            (
                ModulePath::parse("github.com/acme/a").unwrap(),
                DepConstraint::parse("^1.0.0").unwrap(),
            ),
        ];
        let graph = solve(
            "github.com/acme/root",
            &ToolchainConstraint::parse("^1.0.0").unwrap(),
            &dependencies,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();
        assert_eq!(
            graph.modules[&ModulePath::parse("github.com/acme/a").unwrap()]
                .version
                .to_string(),
            "1.1.0"
        );
        assert_eq!(
            graph.modules[&ModulePath::parse("github.com/acme/b").unwrap()]
                .version
                .to_string(),
            "1.0.0"
        );
    }

    #[test]
    fn iterative_search_handles_more_than_256_independent_root_modules() {
        let mut reg = MockRegistry::new();
        let mut dependencies = Vec::new();
        for index in 0..512 {
            let module = format!("github.com/acme/module-{index:04}");
            reg.add_module(&module, "1.0.0", &[]);
            dependencies.push((
                ModulePath::parse(&module).unwrap(),
                DepConstraint::parse("^1.0.0").unwrap(),
            ));
        }
        let graph = solve(
            "github.com/acme/root",
            &ToolchainConstraint::parse("^1.0.0").unwrap(),
            &dependencies,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();
        assert_eq!(graph.modules.len(), 512);
    }

    #[test]
    fn solve_freezes_registry_reads_across_search_branches() {
        let mut reg = MockRegistry::new();
        reg.add_module(
            "github.com/acme/a",
            "1.0.0",
            &[
                ("github.com/acme/b", "^1.0.0"),
                ("github.com/acme/c", "^1.0.0"),
            ],
        );
        reg.add_module(
            "github.com/acme/b",
            "1.0.0",
            &[("github.com/acme/c", "~1.1.0")],
        );
        reg.add_module(
            "github.com/acme/b",
            "1.1.0",
            &[("github.com/acme/c", "~1.2.0")],
        );
        reg.add_module("github.com/acme/c", "1.0.0", &[]);
        reg.add_module("github.com/acme/c", "1.1.0", &[]);
        // Duplicate index metadata must normalize to one candidate after the
        // untrusted-list size limit has been enforced.
        reg.add_module("github.com/acme/c", "1.1.0", &[]);
        reg.add_module("github.com/acme/c", "1.2.0", &[]);
        // The highest C release is incomplete. Its retained parse failure is
        // reused when the first B branch narrows C to ~1.2.0.
        reg.set_manifest_raw("github.com/acme/c", "1.2.0", b"{invalid");
        reg.reject_repeat_reads();

        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/a").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];
        let graph = solve(
            "github.com/acme/app",
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();

        assert_eq!(
            graph.modules[&ModulePath::parse("github.com/acme/b").unwrap()]
                .version
                .to_string(),
            "1.0.0"
        );
        assert_eq!(
            graph.modules[&ModulePath::parse("github.com/acme/c").unwrap()]
                .version
                .to_string(),
            "1.1.0"
        );
        for module in [
            "github.com/acme/a",
            "github.com/acme/b",
            "github.com/acme/c",
        ] {
            assert_eq!(reg.list_call_count(module), 1, "list calls for {module}");
        }
        for (module, version) in [
            ("github.com/acme/a", "1.0.0"),
            ("github.com/acme/b", "1.0.0"),
            ("github.com/acme/b", "1.1.0"),
            ("github.com/acme/c", "1.1.0"),
            ("github.com/acme/c", "1.2.0"),
        ] {
            assert_eq!(
                reg.manifest_call_count(module, version),
                1,
                "manifest calls for {module} {version}"
            );
        }
        assert_eq!(
            reg.manifest_call_count("github.com/acme/c", "1.0.0"),
            0,
            "versions excluded by accumulated constraints are never fetched"
        );
    }

    #[test]
    fn solver_snapshot_preserves_cached_error_variants_and_text() {
        let mut reg = MockRegistry::new();
        reg.fail_version_list_with_network("github.com/acme/offline", "offline exactly");
        reg.add_module("github.com/acme/broken", "1.0.0", &[]);
        reg.set_manifest_raw("github.com/acme/broken", "1.0.0", b"{invalid");
        let snapshot = SolverRegistrySnapshot::new(&reg, SolveLimits::default());

        let offline = ModulePath::parse("github.com/acme/offline").unwrap();
        let first = snapshot.version_candidates(&offline).unwrap_err();
        let second = snapshot.version_candidates(&offline).unwrap_err();
        assert!(matches!(&first, Error::Network(_)));
        assert!(matches!(&second, Error::Network(_)));
        assert_eq!(first.to_string(), second.to_string());
        assert_eq!(reg.list_call_count(offline.as_str()), 1);

        let broken = ModulePath::parse("github.com/acme/broken").unwrap();
        let version = ExactVersion::parse("1.0.0").unwrap();
        let first = snapshot.manifest(&broken, &version).unwrap_err();
        let second = snapshot.manifest(&broken, &version).unwrap_err();
        assert!(matches!(&first, Error::InvalidReleaseMetadata(_)));
        assert!(matches!(&second, Error::InvalidReleaseMetadata(_)));
        assert_eq!(first.to_string(), second.to_string());
        assert_eq!(reg.manifest_call_count(broken.as_str(), "1.0.0"), 1);

        let frozen = FrozenError(Error::Io(std::io::Error::new(
            std::io::ErrorKind::PermissionDenied,
            "permission denied exactly",
        )));
        let first = frozen.thaw();
        let second = frozen.thaw();
        assert!(
            matches!(&first, Error::Io(error) if error.kind() == std::io::ErrorKind::PermissionDenied)
        );
        assert!(
            matches!(&second, Error::Io(error) if error.kind() == std::io::ErrorKind::PermissionDenied)
        );
        assert_eq!(first.to_string(), second.to_string());
    }

    #[test]
    fn solve_enforces_aggregate_graph_and_search_budgets() {
        let mut reg = MockRegistry::new();
        reg.add_module(
            "github.com/acme/a",
            "1.0.0",
            &[
                ("github.com/acme/b", "^1.0.0"),
                ("github.com/acme/c", "^1.0.0"),
            ],
        );
        reg.add_module("github.com/acme/b", "1.0.0", &[]);
        reg.add_module("github.com/acme/c", "1.0.0", &[]);
        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/a").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];

        let limits = SolveLimits {
            edges: 2,
            ..SolveLimits::default()
        };
        let error = solve_with_limits(
            "github.com/acme/app",
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
            limits,
        )
        .unwrap_err();
        assert!(
            matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 2 } if resource == "selected graph edge count")
        );

        reg.add_wasm_artifact("github.com/acme/a", "1.0.0");
        let limits = SolveLimits {
            artifacts: 0,
            ..SolveLimits::default()
        };
        let error = solve_with_limits(
            "github.com/acme/app",
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
            limits,
        )
        .unwrap_err();
        assert!(
            matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "selected graph artifact count")
        );

        let limits = SolveLimits {
            candidates: 0,
            ..SolveLimits::default()
        };
        let error = solve_with_limits(
            "github.com/acme/app",
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
            limits,
        )
        .unwrap_err();
        assert!(
            matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "retained registry candidate count")
        );

        let limits = SolveLimits {
            manifest_bytes: 0,
            ..SolveLimits::default()
        };
        let error = solve_with_limits(
            "github.com/acme/app",
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
            limits,
        )
        .unwrap_err();
        assert!(
            matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "processed manifest byte count")
        );

        let limits = SolveLimits {
            decisions: 0,
            ..SolveLimits::default()
        };
        let error = solve_with_limits(
            "github.com/acme/app",
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
            limits,
        )
        .unwrap_err();
        assert!(
            matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "search decision count")
        );

        let limits = SolveLimits {
            backtracks: 0,
            ..SolveLimits::default()
        };
        let budget = SearchBudget::new(limits);
        let error = budget.record_backtrack().unwrap_err();
        assert!(
            matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "backtrack count")
        );
    }

    #[test]
    fn malformed_manifests_are_charged_before_parsing() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/lib", "1.0.0", &[]);
        reg.add_module("github.com/acme/lib", "1.1.0", &[]);
        reg.set_manifest_raw("github.com/acme/lib", "1.0.0", b"bad-json");
        reg.set_manifest_raw("github.com/acme/lib", "1.1.0", b"bad-json");
        let limits = SolveLimits {
            manifest_bytes: b"bad-json".len(),
            ..SolveLimits::default()
        };

        let error = solve_with_limits(
            "github.com/acme/app",
            &ToolchainConstraint::parse("^1.0.0").unwrap(),
            &[(
                ModulePath::parse("github.com/acme/lib").unwrap(),
                DepConstraint::parse("^1.0.0").unwrap(),
            )],
            &reg,
            &SolvePreferences::default(),
            limits,
        )
        .unwrap_err();

        assert!(
            matches!(error, Error::ResolutionLimitExceeded { ref resource, limit } if resource == "processed manifest byte count" && limit == b"bad-json".len())
        );
        assert_eq!(reg.manifest_call_count("github.com/acme/lib", "1.1.0"), 1);
        assert_eq!(reg.manifest_call_count("github.com/acme/lib", "1.0.0"), 1);
    }

    #[test]
    fn transient_highest_manifest_failure_does_not_downgrade_resolution() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/lib", "1.0.0", &[]);
        reg.add_module("github.com/acme/lib", "1.1.0", &[]);
        reg.fail_manifest_with_network(
            "github.com/acme/lib",
            "1.1.0",
            "highest manifest unavailable",
        );
        let error = solve(
            "github.com/acme/app",
            &ToolchainConstraint::parse("^1.0.0").unwrap(),
            &[(
                (ModulePath::parse("github.com/acme/lib").unwrap()),
                DepConstraint::parse("^1.0.0").unwrap(),
            )],
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap_err();
        assert!(
            matches!(&error, Error::Network(message) if message == "highest manifest unavailable")
        );
    }

    #[test]
    fn transitive_infrastructure_failure_aborts_instead_of_trying_an_older_parent() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/parent", "1.0.0", &[]);
        reg.add_module(
            "github.com/acme/parent",
            "1.1.0",
            &[("github.com/acme/offline", "^1.0.0")],
        );
        reg.fail_version_list_with_network("github.com/acme/offline", "offline exactly");

        let error = solve(
            "github.com/acme/app",
            &ToolchainConstraint::parse("^1.0.0").unwrap(),
            &[(
                ModulePath::parse("github.com/acme/parent").unwrap(),
                DepConstraint::parse("^1.0.0").unwrap(),
            )],
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap_err();

        assert!(matches!(&error, Error::Network(message) if message == "offline exactly"));
        assert_eq!(reg.list_call_count("github.com/acme/offline"), 1);
    }

    #[test]
    fn definitive_invalid_highest_manifest_can_be_skipped() {
        let mut reg = MockRegistry::new();
        reg.add_module("github.com/acme/lib", "1.0.0", &[]);
        reg.add_module("github.com/acme/lib", "1.1.0", &[]);
        reg.set_manifest_raw("github.com/acme/lib", "1.1.0", b"{invalid");
        let graph = solve(
            "github.com/acme/app",
            &ToolchainConstraint::parse("^1.0.0").unwrap(),
            &[(
                (ModulePath::parse("github.com/acme/lib").unwrap()),
                DepConstraint::parse("^1.0.0").unwrap(),
            )],
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap();
        assert_eq!(
            graph.modules[&ModulePath::parse("github.com/acme/lib").unwrap()]
                .version
                .to_string(),
            "1.0.0"
        );
    }

    #[test]
    fn solve_rejects_dependency_toolchain_range_with_older_prerelease_core() {
        let mut reg = MockRegistry::new();
        reg.add_module_with_vo("github.com/acme/lib", "1.0.0", "^1.0.0-alpha.1", &[]);
        let root_vo = ToolchainConstraint::parse("^1.1.0-alpha.1").unwrap();
        let reqs = vec![(
            ModulePath::parse("github.com/acme/lib").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )];
        let error = solve(
            "github.com/acme/app",
            &root_vo,
            &reqs,
            &reg,
            &SolvePreferences::default(),
        )
        .unwrap_err();
        assert!(matches!(error, Error::DependencyToolchainMismatch { .. }));
    }

    #[test]
    fn test_solve_toolchain_subset_ok() {
        let mut reg = MockRegistry::new();
        // Root ^1.0.0 is subset of dep ^1.0.0 — should succeed.
        reg.add_module_with_vo("github.com/acme/lib", "1.0.0", "^1.0.0", &[]);

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
