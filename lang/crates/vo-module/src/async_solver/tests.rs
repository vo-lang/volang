use super::*;
use std::future::Future;
use std::sync::Mutex;
use std::task::{Context, Poll, Waker};

use crate::async_install::{BoxFuture, SourcePayload};
use crate::digest::Digest;
use crate::identity::ArtifactId;
use crate::registry::Registry;
use crate::schema::manifest::{
    ManifestArtifact, ManifestRequire, ManifestSource, ManifestWebManifest, ReleaseManifest,
};

struct MockRegistry {
    versions: BTreeMap<String, Vec<ExactVersion>>,
    manifests: BTreeMap<(String, String), Vec<u8>>,
    list_failures: BTreeMap<String, Error>,
    manifest_failures: BTreeMap<(String, String), Error>,
    list_calls: Mutex<BTreeMap<String, usize>>,
    manifest_calls: Mutex<BTreeMap<(String, String), usize>>,
    reject_repeat_reads: bool,
}

impl MockRegistry {
    fn new() -> Self {
        Self {
            versions: BTreeMap::new(),
            manifests: BTreeMap::new(),
            list_failures: BTreeMap::new(),
            manifest_failures: BTreeMap::new(),
            list_calls: Mutex::new(BTreeMap::new()),
            manifest_calls: Mutex::new(BTreeMap::new()),
            reject_repeat_reads: false,
        }
    }

    fn add_module(&mut self, module: &str, version: &str, deps: &[(&str, &str)]) {
        self.add_module_with_vo(module, version, "^1.0.0", deps);
    }

    fn add_module_with_vo(&mut self, module: &str, version: &str, vo: &str, deps: &[(&str, &str)]) {
        let module_path = ModulePath::parse(module).unwrap();
        let exact_version = ExactVersion::parse(version).unwrap();
        self.versions
            .entry(module.to_string())
            .or_default()
            .push(exact_version.clone());
        let mut require = deps
            .iter()
            .map(|(dependency, constraint)| ManifestRequire {
                module: ModulePath::parse(dependency).unwrap(),
                constraint: DepConstraint::parse(constraint).unwrap(),
            })
            .collect::<Vec<_>>();
        require.sort_by(|left, right| left.module.cmp(&right.module));
        let manifest = ReleaseManifest {
            schema_version: 1,
            module: module_path.clone(),
            version: exact_version.clone(),
            commit: "a".repeat(40),
            module_root: module_path.module_root().to_string(),
            vo: ToolchainConstraint::parse(vo).unwrap(),
            require,
            source: ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 1,
                digest: Digest::from_sha256(b"source"),
                files_size: 1,
                files_digest: Digest::from_sha256(b"files"),
            },
            web_manifest: ManifestWebManifest {
                size: 3,
                digest: Digest::from_sha256(b"{}\n"),
            },
            artifacts: Vec::new(),
        };
        self.manifests.insert(
            (module.to_string(), version.to_string()),
            manifest.render().unwrap().into_bytes(),
        );
    }

    fn set_manifest_raw(&mut self, module: &str, version: &str, raw: &[u8]) {
        self.manifests
            .insert((module.to_string(), version.to_string()), raw.to_vec());
    }

    fn add_wasm_artifact(&mut self, module: &str, version: &str) {
        let raw = self
            .manifests
            .get_mut(&(module.to_string(), version.to_string()))
            .unwrap();
        let mut manifest = ReleaseManifest::parse(std::str::from_utf8(raw).unwrap()).unwrap();
        manifest.artifacts.push(ManifestArtifact {
            id: ArtifactId {
                kind: "extension-wasm".to_string(),
                target: "wasm32-unknown-unknown".to_string(),
                name: "extension.wasm".to_string(),
            },
            size: 1,
            digest: Digest::from_sha256(b"artifact"),
        });
        *raw = manifest.render().unwrap().into_bytes();
    }

    fn fail_list(&mut self, module: &str, error: Error) {
        self.list_failures.insert(module.to_string(), error);
    }

    fn fail_manifest(&mut self, module: &str, version: &str, error: Error) {
        self.manifest_failures
            .insert((module.to_string(), version.to_string()), error);
    }

    fn reject_repeat_reads(&mut self) {
        self.reject_repeat_reads = true;
    }

    fn list_call_count(&self, module: &str) -> usize {
        self.list_calls
            .lock()
            .unwrap()
            .get(module)
            .copied()
            .unwrap_or(0)
    }

    fn manifest_call_count(&self, module: &str, version: &str) -> usize {
        self.manifest_calls
            .lock()
            .unwrap()
            .get(&(module.to_string(), version.to_string()))
            .copied()
            .unwrap_or(0)
    }

    fn list(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        let count = {
            let mut calls = self.list_calls.lock().unwrap();
            let count = calls.entry(module.as_str().to_string()).or_default();
            *count += 1;
            *count
        };
        if self.reject_repeat_reads && count > 1 {
            return Err(Error::RegistryError(format!(
                "version index changed after first read for {module}"
            )));
        }
        if let Some(error) = self.list_failures.get(module.as_str()) {
            return Err(error.clone());
        }
        Ok(self
            .versions
            .get(module.as_str())
            .cloned()
            .unwrap_or_default())
    }

    fn manifest(&self, module: &ModulePath, version: &ExactVersion) -> Result<Vec<u8>, Error> {
        let key = (module.as_str().to_string(), version.to_string());
        let count = {
            let mut calls = self.manifest_calls.lock().unwrap();
            let count = calls.entry(key.clone()).or_default();
            *count += 1;
            *count
        };
        if self.reject_repeat_reads && count > 1 {
            return Err(Error::RegistryError(format!(
                "manifest changed after first read for {module} {version}"
            )));
        }
        if let Some(error) = self.manifest_failures.get(&key) {
            return Err(error.clone());
        }
        self.manifests
            .get(&key)
            .cloned()
            .ok_or_else(|| Error::RegistryError(format!("missing manifest for {module} {version}")))
    }
}

impl AsyncRegistry for MockRegistry {
    fn list_version_candidates<'a>(
        &'a self,
        module: &'a ModulePath,
    ) -> BoxFuture<'a, Result<Vec<ExactVersion>, Error>> {
        let result = self.list(module);
        Box::pin(async move { result })
    }

    fn fetch_manifest_raw<'a>(
        &'a self,
        module: &'a ModulePath,
        version: &'a ExactVersion,
    ) -> BoxFuture<'a, Result<Vec<u8>, Error>> {
        let result = self.manifest(module, version);
        Box::pin(async move { result })
    }

    fn fetch_source<'a>(
        &'a self,
        _module: &'a ModulePath,
        _version: &'a ExactVersion,
        _asset_name: &'a str,
    ) -> BoxFuture<'a, Result<SourcePayload, Error>> {
        Box::pin(async {
            Err(Error::RegistryError(
                "unexpected source fetch in solver test".to_string(),
            ))
        })
    }

    fn fetch_artifact<'a>(
        &'a self,
        _module: &'a ModulePath,
        _version: &'a ExactVersion,
        _artifact: &'a ArtifactId,
    ) -> BoxFuture<'a, Result<Vec<u8>, Error>> {
        Box::pin(async {
            Err(Error::RegistryError(
                "unexpected artifact fetch in solver test".to_string(),
            ))
        })
    }
}

impl Registry for MockRegistry {
    fn list_version_candidates(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        self.list(module)
    }

    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        self.manifest(module, version)
    }

    fn fetch_source_package(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        Err(Error::RegistryError(
            "unexpected source fetch in solver test".to_string(),
        ))
    }

    fn fetch_artifact(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _artifact: &ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        Err(Error::RegistryError(
            "unexpected artifact fetch in solver test".to_string(),
        ))
    }
}

fn poll_ready<F: Future>(future: F) -> F::Output {
    let mut future = Box::pin(future);
    let waker = Waker::noop();
    let mut context = Context::from_waker(waker);
    match future.as_mut().poll(&mut context) {
        Poll::Ready(output) => output,
        Poll::Pending => panic!("mock registry future unexpectedly yielded"),
    }
}

fn requirement(module: &str, constraint: &str) -> RootRequirement {
    RootRequirement {
        module: ModulePath::parse(module).unwrap(),
        constraint: Some(DepConstraint::parse(constraint).unwrap()),
    }
}

fn parity_registry() -> MockRegistry {
    let mut registry = MockRegistry::new();
    registry.add_module(
        "github.com/acme/a",
        "v1.0.0",
        &[
            ("github.com/acme/b", "^1.0.0"),
            ("github.com/acme/c", "^1.0.0"),
        ],
    );
    registry.add_module(
        "github.com/acme/b",
        "v1.1.0",
        &[("github.com/acme/c", "~1.2.0")],
    );
    registry.add_module(
        "github.com/acme/b",
        "v1.0.0",
        &[("github.com/acme/c", "~1.1.0")],
    );
    registry.add_module("github.com/acme/c", "v1.0.0", &[]);
    registry.add_module("github.com/acme/c", "v1.1.0", &[]);
    registry.add_module("github.com/acme/c", "v1.1.0", &[]);
    registry.add_module("github.com/acme/c", "v1.2.0", &[]);
    registry.set_manifest_raw("github.com/acme/c", "v1.2.0", b"{invalid");
    registry
}

fn graph_versions(graph: &ResolvedGraph) -> BTreeMap<String, String> {
    graph
        .modules
        .iter()
        .map(|(module, resolved)| (module.to_string(), resolved.version.to_string()))
        .collect()
}

#[test]
fn native_and_async_solvers_select_the_same_backtracked_graph() {
    let native_registry = parity_registry();
    let async_registry = parity_registry();
    let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
    let native = crate::solver::solve(
        "github.com/acme/root",
        &root_vo,
        &[(
            ModulePath::parse("github.com/acme/a").unwrap(),
            DepConstraint::parse("^1.0.0").unwrap(),
        )],
        &native_registry,
        &crate::solver::SolvePreferences::default(),
    )
    .unwrap();
    let asynchronous = poll_ready(solve(
        "github.com/acme/root",
        Some(&root_vo),
        &[requirement("github.com/acme/a", "^1.0.0")],
        &async_registry,
        &BTreeMap::new(),
    ))
    .unwrap();

    assert_eq!(graph_versions(&native), graph_versions(&asynchronous));
    assert_eq!(graph_versions(&asynchronous)["github.com/acme/b"], "v1.0.0");
    assert_eq!(graph_versions(&asynchronous)["github.com/acme/c"], "v1.1.0");
}

#[test]
fn async_search_freezes_lists_manifests_raw_bytes_and_errors() {
    let mut registry = parity_registry();
    registry.fail_list(
        "github.com/acme/offline",
        Error::Network("list failure exactly".to_string()),
    );
    registry.fail_manifest(
        "github.com/acme/a",
        "v1.0.0",
        Error::Network("manifest failure exactly".to_string()),
    );
    registry.reject_repeat_reads();
    let mut snapshot = RegistrySnapshot::new(&registry, SolveLimits::default());

    let c = ModulePath::parse("github.com/acme/c").unwrap();
    let first_versions = poll_ready(snapshot.version_candidates(&c)).unwrap();
    let second_versions = poll_ready(snapshot.version_candidates(&c)).unwrap();
    assert!(Arc::ptr_eq(&first_versions, &second_versions));
    assert_eq!(registry.list_call_count(c.as_str()), 1);

    let c_version = ExactVersion::parse("v1.1.0").unwrap();
    let expected_raw = registry
        .manifests
        .get(&(c.to_string(), c_version.to_string()))
        .unwrap()
        .clone();
    let first_manifest = poll_ready(snapshot.manifest(&c, &c_version)).unwrap();
    let second_manifest = poll_ready(snapshot.manifest(&c, &c_version)).unwrap();
    assert!(Arc::ptr_eq(&first_manifest, &second_manifest));
    assert_eq!(first_manifest.raw.as_ref(), expected_raw.as_slice());
    assert_eq!(registry.manifest_call_count(c.as_str(), "v1.1.0"), 1);

    let offline = ModulePath::parse("github.com/acme/offline").unwrap();
    for _ in 0..2 {
        let error = poll_ready(snapshot.version_candidates(&offline)).unwrap_err();
        assert!(matches!(&error, Error::Network(message) if message == "list failure exactly"));
    }
    assert_eq!(registry.list_call_count(offline.as_str()), 1);

    let a = ModulePath::parse("github.com/acme/a").unwrap();
    let a_version = ExactVersion::parse("v1.0.0").unwrap();
    for _ in 0..2 {
        let error = poll_ready(snapshot.manifest(&a, &a_version)).unwrap_err();
        assert!(matches!(&error, Error::Network(message) if message == "manifest failure exactly"));
    }
    assert_eq!(registry.manifest_call_count(a.as_str(), "v1.0.0"), 1);
}

#[test]
fn network_and_io_manifest_failures_never_downgrade() {
    for error in [
        Error::Network("transient network failure".to_string()),
        Error::Io(std::io::Error::other("transient io failure")),
    ] {
        let mut registry = MockRegistry::new();
        registry.add_module("github.com/acme/lib", "v1.0.0", &[]);
        registry.add_module("github.com/acme/lib", "v1.1.0", &[]);
        registry.fail_manifest("github.com/acme/lib", "v1.1.0", error.clone());
        let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
        let found = poll_ready(solve(
            "github.com/acme/root",
            Some(&root_vo),
            &[requirement("github.com/acme/lib", "^1.0.0")],
            &registry,
            &BTreeMap::new(),
        ))
        .unwrap_err();
        assert_eq!(found.to_string(), error.to_string());
        assert_eq!(
            registry.manifest_call_count("github.com/acme/lib", "v1.0.0"),
            0
        );
    }
}

#[test]
fn transitive_infrastructure_failure_aborts_parent_backtracking() {
    let mut registry = MockRegistry::new();
    registry.add_module("github.com/acme/parent", "v1.0.0", &[]);
    registry.add_module(
        "github.com/acme/parent",
        "v1.1.0",
        &[("github.com/acme/offline", "^1.0.0")],
    );
    registry.fail_list(
        "github.com/acme/offline",
        Error::Network("offline exactly".to_string()),
    );
    let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
    let error = poll_ready(solve(
        "github.com/acme/root",
        Some(&root_vo),
        &[requirement("github.com/acme/parent", "^1.0.0")],
        &registry,
        &BTreeMap::new(),
    ))
    .unwrap_err();
    assert!(matches!(&error, Error::Network(message) if message == "offline exactly"));
    assert_eq!(registry.list_call_count("github.com/acme/offline"), 1);
}

#[test]
fn immutable_invalid_release_is_skipped_and_selected_raw_bytes_are_exact() {
    let mut registry = MockRegistry::new();
    registry.add_module("github.com/acme/lib", "v1.0.0", &[]);
    registry.add_module("github.com/acme/lib", "v1.1.0", &[]);
    registry.set_manifest_raw("github.com/acme/lib", "v1.1.0", b"{invalid");
    let selected_raw = registry
        .manifests
        .get(&("github.com/acme/lib".to_string(), "v1.0.0".to_string()))
        .unwrap()
        .clone();
    let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
    let graph = poll_ready(solve(
        "github.com/acme/root",
        Some(&root_vo),
        &[requirement("github.com/acme/lib", "^1.0.0")],
        &registry,
        &BTreeMap::new(),
    ))
    .unwrap();
    let selected = &graph.modules[&ModulePath::parse("github.com/acme/lib").unwrap()];
    assert_eq!(selected.version.to_string(), "v1.0.0");
    assert_eq!(selected.manifest_raw, selected_raw);
    assert_eq!(
        registry.manifest_call_count("github.com/acme/lib", "v1.1.0"),
        1
    );
    assert_eq!(
        registry.manifest_call_count("github.com/acme/lib", "v1.0.0"),
        1
    );
}

#[test]
fn async_solver_enforces_global_budgets() {
    let mut registry = MockRegistry::new();
    registry.add_module("github.com/acme/lib", "v1.0.0", &[]);
    let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
    let requirements = [requirement("github.com/acme/lib", "^1.0.0")];

    let limits = SolveLimits {
        candidates: 0,
        ..SolveLimits::default()
    };
    let error = poll_ready(solve_with_limits(
        "github.com/acme/root",
        Some(&root_vo),
        &requirements,
        &registry,
        &BTreeMap::new(),
        limits,
    ))
    .unwrap_err();
    assert!(
        matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "retained registry candidate count")
    );

    registry.add_wasm_artifact("github.com/acme/lib", "v1.0.0");
    let limits = SolveLimits {
        artifacts: 0,
        ..SolveLimits::default()
    };
    let error = poll_ready(solve_with_limits(
        "github.com/acme/root",
        Some(&root_vo),
        &requirements,
        &registry,
        &BTreeMap::new(),
        limits,
    ))
    .unwrap_err();
    assert!(
        matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "selected graph artifact count")
    );

    let limits = SolveLimits {
        manifest_bytes: 0,
        ..SolveLimits::default()
    };
    let error = poll_ready(solve_with_limits(
        "github.com/acme/root",
        Some(&root_vo),
        &requirements,
        &registry,
        &BTreeMap::new(),
        limits,
    ))
    .unwrap_err();
    assert!(
        matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "processed manifest byte count")
    );

    let limits = SolveLimits {
        decisions: 0,
        ..SolveLimits::default()
    };
    let error = poll_ready(solve_with_limits(
        "github.com/acme/root",
        Some(&root_vo),
        &requirements,
        &registry,
        &BTreeMap::new(),
        limits,
    ))
    .unwrap_err();
    assert!(
        matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "search decision count")
    );

    let limits = SolveLimits {
        backtracks: 0,
        ..SolveLimits::default()
    };
    let mut budget = SearchBudget::new(limits);
    let error = budget.record_backtrack().unwrap_err();
    assert!(
        matches!(error, Error::ResolutionLimitExceeded { ref resource, limit: 0 } if resource == "backtrack count")
    );
}

#[test]
fn async_malformed_manifests_are_charged_before_parsing() {
    let mut registry = MockRegistry::new();
    registry.add_module("github.com/acme/lib", "v1.0.0", &[]);
    registry.add_module("github.com/acme/lib", "v1.1.0", &[]);
    registry.set_manifest_raw("github.com/acme/lib", "v1.0.0", b"bad-json");
    registry.set_manifest_raw("github.com/acme/lib", "v1.1.0", b"bad-json");
    let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
    let requirements = [requirement("github.com/acme/lib", "^1.0.0")];
    let limits = SolveLimits {
        manifest_bytes: b"bad-json".len(),
        ..SolveLimits::default()
    };

    let error = poll_ready(solve_with_limits(
        "github.com/acme/root",
        Some(&root_vo),
        &requirements,
        &registry,
        &BTreeMap::new(),
        limits,
    ))
    .unwrap_err();

    assert!(
        matches!(error, Error::ResolutionLimitExceeded { ref resource, limit } if resource == "processed manifest byte count" && limit == b"bad-json".len())
    );
    assert_eq!(
        registry.manifest_call_count("github.com/acme/lib", "v1.1.0"),
        1
    );
    assert_eq!(
        registry.manifest_call_count("github.com/acme/lib", "v1.0.0"),
        1
    );
}

#[test]
fn iterative_async_search_handles_more_than_256_root_modules() {
    let mut registry = MockRegistry::new();
    let mut requirements = Vec::new();
    for index in 0..300 {
        let module = format!("github.com/acme/module-{index:04}");
        registry.add_module(&module, "v1.0.0", &[]);
        requirements.push(requirement(&module, "^1.0.0"));
    }
    let root_vo = ToolchainConstraint::parse("^1.0.0").unwrap();
    let graph = poll_ready(solve(
        "github.com/acme/root",
        Some(&root_vo),
        &requirements,
        &registry,
        &BTreeMap::new(),
    ))
    .unwrap();
    assert_eq!(graph.modules.len(), 300);
}
