use std::collections::BTreeSet;
use std::ffi::OsStr;
use std::io;
use std::path::Path;
use std::path::PathBuf;

use vo_common::vfs::{normalize_fs_path, FileSystemEntryKind};

use crate::identity::ModulePath;
use crate::registry::Registry;
use crate::schema::lockfile::LockFile;
use crate::schema::modfile::ModFile;
use crate::solver::{self, ResolvedGraph, SolvePreferences};
use crate::version::{ExactVersion, ToolchainConstraint};
use crate::Error;

pub fn download_locked_dependencies(
    cache_root: &Path,
    lock_file: &LockFile,
    registry: &dyn Registry,
) -> Result<(), Error> {
    crate::schema::lockfile::validate_locked_module_graph(&lock_file.modules)?;
    crate::cache::install::populate_locked_cache(cache_root, lock_file, registry)
}

/// Download the registry-backed subset selected by a validated project
/// context. Workspace sources may remove vertices from materialization,
/// while the complete lock authority remains encapsulated by `ProjectPlan`.
pub fn download_project_dependencies(
    cache_root: &Path,
    project_plan: &crate::project::ProjectPlan,
    registry: &dyn Registry,
) -> Result<(), Error> {
    crate::cache::install::populate_locked_modules(
        cache_root,
        project_plan.locked_modules(),
        registry,
    )
}

pub(crate) fn prepare_lock_file(
    mod_file: &ModFile,
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<Option<LockFile>, Error> {
    // Typed callers can construct and mutate `ModFile` values without going
    // through the parser. Reject an invalid project graph before registry or
    // cache I/O begins.
    mod_file.validate()?;
    let reqs = solved_requirements(mod_file);
    if reqs.is_empty() {
        return Ok(None);
    }
    let graph = solve_from_requirements(mod_file, &reqs, registry, prefs)?;
    Ok(Some(crate::lock::generate_lock(mod_file, &graph)?))
}

pub(crate) fn verify_locked_dependencies(
    cache_root: &Path,
    mod_file: &ModFile,
    lock_file: &LockFile,
) -> Result<(), Error> {
    crate::lock::verify_root_consistency(mod_file, lock_file)?;
    crate::lock::verify_graph_completeness(mod_file, lock_file)?;
    crate::cache::install::verify_locked_cache(cache_root, lock_file)
}

pub(crate) fn latest_supported_dependency_version(
    project_vo: &ToolchainConstraint,
    module: &ModulePath,
    registry: &dyn Registry,
) -> Result<ExactVersion, Error> {
    latest_supported_dependency_version_with_limit(
        project_vo,
        module,
        registry,
        crate::MAX_SOLVER_MANIFEST_BYTES,
    )
}

fn latest_supported_dependency_version_with_limit(
    project_vo: &ToolchainConstraint,
    module: &ModulePath,
    registry: &dyn Registry,
    manifest_byte_limit: usize,
) -> Result<ExactVersion, Error> {
    let versions = crate::registry::normalize_version_candidates(
        module,
        registry.list_version_candidates(module)?,
    )?;
    let compatible = crate::registry::filter_compatible_versions(module, &versions)
        .into_iter()
        .filter(|version| !version.semver().is_prerelease())
        .collect::<Vec<_>>();
    let mut first_toolchain_mismatch: Option<String> = None;
    let mut first_invalid_candidate = None;
    let mut processed_manifest_bytes = 0usize;
    for version in compatible {
        let manifest = match registry.fetch_manifest_raw(module, &version) {
            Ok(raw) => {
                processed_manifest_bytes = crate::registry::charge_processed_manifest_bytes(
                    processed_manifest_bytes,
                    raw.len(),
                    manifest_byte_limit,
                )?;
                match crate::registry::parse_manifest_bytes(&raw, module, &version) {
                    Ok(manifest) => manifest,
                    Err(error) if crate::registry::is_definitive_invalid_release_error(&error) => {
                        if first_invalid_candidate.is_none() {
                            first_invalid_candidate = Some(error);
                        }
                        continue;
                    }
                    Err(error) => return Err(error),
                }
            }
            Err(error) if crate::registry::is_definitive_invalid_release_error(&error) => {
                if first_invalid_candidate.is_none() {
                    first_invalid_candidate = Some(error);
                }
                continue;
            }
            Err(error) => return Err(error),
        };
        if project_vo.is_subset_of(&manifest.vo) {
            return Ok(version);
        }
        if first_toolchain_mismatch.is_none() {
            first_toolchain_mismatch = Some(manifest.vo.to_string());
        }
    }

    if let Some(error) = first_invalid_candidate {
        return Err(error);
    }
    if let Some(dependency_constraint) = first_toolchain_mismatch {
        return Err(Error::DependencyToolchainMismatch {
            module: module.as_str().to_string(),
            project_constraint: project_vo.to_string(),
            dependency_constraint,
        });
    }
    Err(Error::NoSatisfyingVersion {
        module: module.as_str().to_string(),
        detail: "no non-prerelease versions found".to_string(),
    })
}

pub(crate) fn infer_module_path(
    import_path: &str,
    registry: &dyn Registry,
) -> Result<ModulePath, Error> {
    if crate::identity::classify_import(import_path)? != crate::identity::ImportClass::External {
        return Err(Error::InvalidImportPath(format!(
            "cannot infer an external module from import path: {import_path}"
        )));
    }
    let segments: Vec<&str> = import_path.split('/').collect();
    if segments.len() < 3 {
        return Err(Error::InvalidImportPath(format!(
            "cannot infer module from import path: {import_path}"
        )));
    }

    for end in (3..=segments.len()).rev() {
        let candidate_str = segments[..end].join("/");
        let candidate = match ModulePath::parse(&candidate_str) {
            Ok(candidate) => candidate,
            Err(_) => continue,
        };
        match registry.probe_module_path(&candidate) {
            Ok(true) => return Ok(candidate),
            Ok(false) => {}
            Err(error) => return Err(error),
        }
    }

    Err(Error::NoSatisfyingVersion {
        module: import_path.to_string(),
        detail: "could not infer a published owning module for import".to_string(),
    })
}

pub(crate) fn clean_cache(cache_root: &Path, lock_file: Option<&LockFile>) -> Result<u64, Error> {
    crate::cache::mutation_lock::require_secure_cache_mutation_support()?;
    let cache_root = normalize_fs_path(cache_root);
    match exact_entry_kind(&cache_root)? {
        FileSystemEntryKind::Missing => return Ok(0),
        FileSystemEntryKind::Directory => {}
        other => {
            return Err(invalid_cache_state(format!(
                "module cache root {} must be a directory without symbolic links; found {other:?}",
                cache_root.display(),
            )));
        }
    }
    let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::exclusive(&cache_root)?;
    let cache_directory = mutation_lock.cache_root_directory()?;
    cache_directory.require_path_identity(&cache_root, "module cache root")?;
    let staging_directory = mutation_lock.open_locked_staging_directory()?;

    let locked_dirs: BTreeSet<PathBuf> = lock_file
        .map(|lock_file| {
            lock_file
                .modules
                .iter()
                .map(|locked| {
                    crate::cache::layout::cache_dir(&cache_root, &locked.path, &locked.version)
                })
                .collect()
        })
        .unwrap_or_default();

    let mut removed = 0u64;
    let mut found_staging_directory = false;
    let mut found_owner_marker = false;
    for module_name in cache_directory.entries()? {
        mutation_lock.validate_cache_ownership()?;
        let module_dir = cache_root.join(&module_name);
        if module_name == OsStr::new(crate::cache::mutation_lock::CACHE_OWNER_MARKER) {
            mutation_lock.validate_cache_ownership()?;
            found_owner_marker = true;
            continue;
        }
        if module_name == OsStr::new(crate::cache::layout::STAGING_DIR) {
            if cache_directory.entry_kind(&module_name)? != FileSystemEntryKind::Directory {
                return Err(invalid_cache_state(format!(
                    "module cache staging entry {} must be a directory without symbolic links",
                    module_dir.display(),
                )));
            }
            cache_directory.require_child_identity(
                &module_name,
                &staging_directory,
                "module cache staging directory",
            )?;
            clean_staging_area(&module_dir, &staging_directory, &mutation_lock)?;
            mutation_lock.validate_cache_ownership()?;
            found_staging_directory = true;
            continue;
        }
        if module_name == OsStr::new("ephemeral") {
            if cache_directory.entry_kind(&module_name)? != FileSystemEntryKind::Directory {
                return Err(invalid_cache_state(format!(
                    "ephemeral cache namespace {} must be a directory without symbolic links",
                    module_dir.display(),
                )));
            }
            mutation_lock.validate_cache_ownership()?;
            cache_directory.remove_tree(&module_name, "ephemeral module cache")?;
            mutation_lock.validate_cache_ownership()?;
            continue;
        }
        let module_kind = cache_directory.entry_kind(&module_name)?;
        if module_kind != FileSystemEntryKind::Directory {
            return Err(invalid_cache_state(format!(
                "module cache key {} must be a directory without symbolic links; found {module_kind:?}",
                module_dir.display(),
            )));
        }
        let module_directory = cache_directory.open_child(&module_name, "module cache key")?;
        let module = module_from_cache_key_path(&module_dir)?;

        for version_name in module_directory.entries()? {
            mutation_lock.validate_cache_ownership()?;
            let version_dir = module_dir.join(&version_name);
            let version_kind = module_directory.entry_kind(&version_name)?;
            if version_kind != FileSystemEntryKind::Directory {
                return Err(invalid_cache_state(format!(
                    "module cache version {} must be a directory without symbolic links; found {version_kind:?}",
                    version_dir.display(),
                )));
            }
            let version_name_text = version_name.to_str().ok_or_else(|| {
                invalid_cache_state(format!(
                    "module cache version path is not valid UTF-8: {}",
                    version_dir.display(),
                ))
            })?;
            let version = ExactVersion::parse(version_name_text).map_err(|error| {
                invalid_cache_state(format!(
                    "module cache contains invalid version directory {}: {error}",
                    version_dir.display(),
                ))
            })?;
            let expected = crate::cache::layout::cache_dir(&cache_root, &module, &version);
            if version_dir != expected {
                return Err(invalid_cache_state(format!(
                    "module cache version directory must be {}, found {}",
                    expected.display(),
                    version_dir.display(),
                )));
            }
            if locked_dirs.contains(&version_dir) {
                continue;
            }

            mutation_lock.validate_cache_ownership()?;
            module_directory.remove_tree(&version_name, "module cache version")?;
            mutation_lock.validate_cache_ownership()?;
            removed = removed.checked_add(1).ok_or_else(|| {
                invalid_cache_state("removed module directory count overflows u64".to_string())
            })?;
        }

        let remaining = module_directory.entries()?;
        if remaining.is_empty() {
            mutation_lock.validate_cache_ownership()?;
            cache_directory.remove_empty_directory(
                &module_name,
                &module_directory,
                "module cache key",
            )?;
            mutation_lock.validate_cache_ownership()?;
        }
    }

    if !found_owner_marker {
        return Err(invalid_cache_state(format!(
            "module cache owner marker {} disappeared during cleanup",
            cache_root
                .join(crate::cache::mutation_lock::CACHE_OWNER_MARKER)
                .display(),
        )));
    }

    if !found_staging_directory {
        return Err(invalid_cache_state(format!(
            "module cache staging directory {} disappeared during cleanup",
            cache_root.join(crate::cache::layout::STAGING_DIR).display(),
        )));
    }

    mutation_lock.validate_cache_ownership()?;
    Ok(removed)
}

fn clean_staging_area(
    staging_dir: &Path,
    staging_directory: &crate::cache::mutation_lock::AnchoredDirectory,
    mutation_lock: &crate::cache::mutation_lock::CacheMutationLock,
) -> Result<(), Error> {
    for entry_name in staging_directory.entries()? {
        mutation_lock.validate_cache_ownership()?;
        let entry = staging_dir.join(&entry_name);
        if entry_name == OsStr::new(crate::cache::layout::STAGING_LOCK_FILE) {
            if staging_directory.entry_kind(&entry_name)? != FileSystemEntryKind::RegularFile {
                return Err(invalid_cache_state(format!(
                    "module cache staging lock {} must be a regular file without symbolic links",
                    entry.display(),
                )));
            }
            continue;
        }
        let kind = staging_directory.entry_kind(&entry_name)?;
        if !matches!(
            kind,
            FileSystemEntryKind::Directory | FileSystemEntryKind::RegularFile
        ) {
            return Err(invalid_cache_state(format!(
                "module cache staging entry {} must not be a symbolic link or special file; found {kind:?}",
                entry.display(),
            )));
        }
        mutation_lock.validate_cache_ownership()?;
        match kind {
            FileSystemEntryKind::Directory => {
                staging_directory.remove_tree(&entry_name, "module cache staging directory")?
            }
            FileSystemEntryKind::RegularFile => {
                staging_directory.remove_regular_file(&entry_name, "module cache staging file")?
            }
            other => {
                return Err(invalid_cache_state(format!(
                    "module cache staging entry {} changed to unsafe kind {other:?} before cleanup",
                    entry.display(),
                )));
            }
        }
        mutation_lock.validate_cache_ownership()?;
    }
    Ok(())
}

fn exact_entry_kind(path: &Path) -> io::Result<FileSystemEntryKind> {
    let metadata = match std::fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == io::ErrorKind::NotFound => {
            return Ok(FileSystemEntryKind::Missing);
        }
        Err(error) => return Err(error),
    };
    let file_type = metadata.file_type();
    Ok(if file_type.is_symlink() {
        FileSystemEntryKind::Symlink
    } else if file_type.is_dir() {
        FileSystemEntryKind::Directory
    } else if file_type.is_file() {
        FileSystemEntryKind::RegularFile
    } else {
        FileSystemEntryKind::Special
    })
}

fn module_from_cache_key_path(module_dir: &Path) -> Result<ModulePath, Error> {
    let cache_key = module_dir
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| {
            invalid_cache_state(format!(
                "module cache key is not valid UTF-8: {}",
                module_dir.display(),
            ))
        })?;
    let module = ModulePath::parse(&cache_key.replace('@', "/")).map_err(|error| {
        invalid_cache_state(format!(
            "module cache contains invalid cache key {cache_key:?}: {error}",
        ))
    })?;
    let expected = crate::cache::layout::cache_key(&module);
    if cache_key != expected {
        return Err(invalid_cache_state(format!(
            "module cache key must be {expected:?}, found {cache_key:?}",
        )));
    }
    Ok(module)
}

fn invalid_cache_state(detail: String) -> Error {
    Error::Io(io::Error::new(io::ErrorKind::InvalidData, detail))
}

fn solved_requirements(mod_file: &ModFile) -> Vec<(ModulePath, crate::version::DepConstraint)> {
    mod_file
        .dependencies
        .iter()
        .map(|require| (require.module.clone(), require.constraint.clone()))
        .collect::<Vec<_>>()
}

fn solve_from_requirements(
    mod_file: &ModFile,
    reqs: &[(ModulePath, crate::version::DepConstraint)],
    registry: &dyn Registry,
    prefs: &SolvePreferences,
) -> Result<ResolvedGraph, Error> {
    solver::solve(
        mod_file.module.as_str(),
        &mod_file.vo,
        reqs,
        registry,
        prefs,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use std::sync::Mutex;

    use crate::digest::Digest;
    use crate::identity::ArtifactId;
    use crate::schema::manifest::{ManifestSource, ReleaseManifest};

    struct SelectionRegistry {
        versions: BTreeMap<String, Vec<ExactVersion>>,
        manifests: BTreeMap<(String, String), Vec<u8>>,
        manifest_failures: BTreeMap<(String, String), Error>,
        manifest_calls: Mutex<BTreeMap<(String, String), usize>>,
        probes: BTreeMap<String, Result<bool, Error>>,
        probe_calls: Mutex<BTreeMap<String, usize>>,
    }

    impl SelectionRegistry {
        fn new() -> Self {
            Self {
                versions: BTreeMap::new(),
                manifests: BTreeMap::new(),
                manifest_failures: BTreeMap::new(),
                manifest_calls: Mutex::new(BTreeMap::new()),
                probes: BTreeMap::new(),
                probe_calls: Mutex::new(BTreeMap::new()),
            }
        }

        fn add_module(&mut self, module: &str, version: &str, vo: &str) {
            let module_path = ModulePath::parse(module).unwrap();
            let exact_version = ExactVersion::parse(version).unwrap();
            self.versions
                .entry(module.to_string())
                .or_default()
                .push(exact_version.clone());
            let manifest = ReleaseManifest {
                format: 1,
                module: module_path.clone(),
                version: exact_version,
                vo: ToolchainConstraint::parse(vo).unwrap(),
                intent: Digest::from_sha256(format!("{module}@{version}-intent").as_bytes()),
                dependencies: Vec::new(),
                source: ManifestSource {
                    name: "source.tar.gz".to_string(),
                    size: 1,
                    digest: Digest::from_sha256(b"source"),
                    tree: Digest::from_sha256(b"{}\n"),
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

        fn fail_manifest(&mut self, module: &str, version: &str, error: Error) {
            self.manifest_failures
                .insert((module.to_string(), version.to_string()), error);
        }

        fn manifest_call_count(&self, module: &str, version: &str) -> usize {
            self.manifest_calls
                .lock()
                .unwrap()
                .get(&(module.to_string(), version.to_string()))
                .copied()
                .unwrap_or(0)
        }

        fn set_probe(&mut self, module: &str, result: Result<bool, Error>) {
            self.probes.insert(module.to_string(), result);
        }

        fn probe_call_count(&self, module: &str) -> usize {
            self.probe_calls
                .lock()
                .unwrap()
                .get(module)
                .copied()
                .unwrap_or(0)
        }
    }

    impl Registry for SelectionRegistry {
        fn list_version_candidates(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
            Ok(self
                .versions
                .get(module.as_str())
                .cloned()
                .unwrap_or_default())
        }

        fn probe_module_path(&self, module: &ModulePath) -> Result<bool, Error> {
            let mut calls = self.probe_calls.lock().unwrap();
            *calls.entry(module.to_string()).or_default() += 1;
            self.probes
                .get(module.as_str())
                .cloned()
                .unwrap_or(Ok(false))
        }

        fn fetch_manifest_raw(
            &self,
            module: &ModulePath,
            version: &ExactVersion,
        ) -> Result<Vec<u8>, Error> {
            let key = (module.to_string(), version.to_string());
            let mut calls = self.manifest_calls.lock().unwrap();
            *calls.entry(key.clone()).or_default() += 1;
            drop(calls);
            if let Some(error) = self.manifest_failures.get(&key) {
                return Err(error.clone());
            }
            self.manifests.get(&key).cloned().ok_or_else(|| {
                Error::RegistryError(format!("missing manifest for {module} {version}"))
            })
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("unexpected source fetch".to_string()))
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _artifact: &ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError(
                "unexpected artifact fetch".to_string(),
            ))
        }
    }

    fn initialize_cache_root(root: &Path) {
        drop(crate::cache::acquire_read_lease(root).unwrap());
    }

    #[test]
    fn latest_requirement_propagates_network_and_io_without_downgrading() {
        for failure in [
            Error::Network("network unavailable exactly".to_string()),
            Error::Io(std::io::Error::other("io unavailable exactly")),
        ] {
            let mut registry = SelectionRegistry::new();
            registry.add_module("github.com/acme/lib", "1.0.0", "1.0.0");
            registry.add_module("github.com/acme/lib", "1.1.0", "1.0.0");
            registry.fail_manifest("github.com/acme/lib", "1.1.0", failure.clone());
            let error = latest_supported_dependency_version(
                &ToolchainConstraint::parse("1.0.0").unwrap(),
                &ModulePath::parse("github.com/acme/lib").unwrap(),
                &registry,
            )
            .unwrap_err();
            assert_eq!(error.to_string(), failure.to_string());
            assert_eq!(
                registry.manifest_call_count("github.com/acme/lib", "1.0.0"),
                0
            );
        }
    }

    #[test]
    fn latest_requirement_skips_only_invalid_release_and_prioritizes_it_over_mismatch() {
        let mut registry = SelectionRegistry::new();
        registry.add_module("github.com/acme/lib", "1.0.0", "1.0.0");
        registry.add_module("github.com/acme/lib", "1.1.0", "1.0.0");
        registry.set_manifest_raw("github.com/acme/lib", "1.1.0", b"{invalid");
        let selected = latest_supported_dependency_version(
            &ToolchainConstraint::parse("1.0.0").unwrap(),
            &ModulePath::parse("github.com/acme/lib").unwrap(),
            &registry,
        )
        .unwrap();
        assert_eq!(selected.to_string(), "1.0.0");

        let mut registry = SelectionRegistry::new();
        registry.add_module("github.com/acme/lib", "1.0.0", "2.0.0");
        registry.add_module("github.com/acme/lib", "1.1.0", "1.0.0");
        registry.set_manifest_raw("github.com/acme/lib", "1.1.0", b"{invalid");
        let error = latest_supported_dependency_version(
            &ToolchainConstraint::parse("1.0.0").unwrap(),
            &ModulePath::parse("github.com/acme/lib").unwrap(),
            &registry,
        )
        .unwrap_err();
        assert!(matches!(error, Error::InvalidReleaseMetadata(_)));
    }

    #[test]
    fn latest_requirement_charges_malformed_bytes_before_parsing() {
        let mut registry = SelectionRegistry::new();
        registry.add_module("github.com/acme/lib", "1.0.0", "1.0.0");
        registry.add_module("github.com/acme/lib", "1.1.0", "1.0.0");
        registry.set_manifest_raw("github.com/acme/lib", "1.0.0", b"bad-json");
        registry.set_manifest_raw("github.com/acme/lib", "1.1.0", b"bad-json");
        let error = latest_supported_dependency_version_with_limit(
            &ToolchainConstraint::parse("1.0.0").unwrap(),
            &ModulePath::parse("github.com/acme/lib").unwrap(),
            &registry,
            b"bad-json".len(),
        )
        .unwrap_err();
        assert!(
            matches!(error, Error::ResolutionLimitExceeded { ref resource, limit } if resource == "processed manifest byte count" && limit == b"bad-json".len())
        );
    }

    #[test]
    fn owner_inference_never_crosses_a_longest_prefix_error() {
        let mut registry = SelectionRegistry::new();
        registry.set_probe(
            "github.com/acme/repo/subpkg",
            Err(Error::Network("longest owner unavailable".to_string())),
        );
        registry.set_probe("github.com/acme/repo", Ok(true));

        let error = infer_module_path("github.com/acme/repo/subpkg", &registry).unwrap_err();
        assert!(
            matches!(&error, Error::Network(message) if message == "longest owner unavailable")
        );
        assert_eq!(registry.probe_call_count("github.com/acme/repo/subpkg"), 1);
        assert_eq!(registry.probe_call_count("github.com/acme/repo"), 0);
    }

    #[test]
    fn clean_cache_validates_entries_in_canonical_order() {
        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        std::fs::create_dir(root.path().join("z-invalid")).unwrap();
        std::fs::create_dir(root.path().join("a-invalid")).unwrap();

        let error = clean_cache(root.path(), None).unwrap_err();

        assert!(error.to_string().contains("a-invalid"), "{error}");
    }

    #[test]
    fn clean_cache_removes_owned_staging_without_counting_a_module_version() {
        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let staging = root.path().join(crate::cache::layout::STAGING_DIR);
        std::fs::create_dir_all(staging.join("abandoned/nested")).unwrap();
        std::fs::write(staging.join("abandoned/nested/vo.mod"), "partial").unwrap();

        assert_eq!(clean_cache(root.path(), None).unwrap(), 0);
        assert!(staging.is_dir());
        assert_eq!(
            exact_entry_kind(&crate::cache::mutation_lock::staging_lock_path(root.path())).unwrap(),
            FileSystemEntryKind::RegularFile,
        );
        assert_eq!(std::fs::read_dir(staging).unwrap().count(), 1);
        assert_eq!(
            std::fs::read(
                root.path()
                    .join(crate::cache::mutation_lock::CACHE_OWNER_MARKER)
            )
            .unwrap(),
            b"volang-module-cache-v2\n",
        );
    }

    #[test]
    fn clean_cache_never_adopts_or_mutates_an_unowned_root() {
        let root = tempfile::tempdir().unwrap();
        let victim = root.path().join("github.com@acme@lib/1.2.3/source.vo");
        std::fs::create_dir_all(victim.parent().unwrap()).unwrap();
        std::fs::write(&victim, b"preserve").unwrap();

        let error = clean_cache(root.path(), None).unwrap_err();

        assert!(error.to_string().contains("missing required owner marker"));
        assert_eq!(std::fs::read(victim).unwrap(), b"preserve");
        assert!(!root.path().join(crate::cache::layout::STAGING_DIR).exists());
        assert!(!root
            .path()
            .join(crate::cache::mutation_lock::CACHE_OWNER_MARKER)
            .exists());
    }

    #[test]
    fn clean_cache_removes_the_reserved_ephemeral_namespace_and_keeps_ownership() {
        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let ephemeral = root.path().join("ephemeral/hash");
        std::fs::create_dir_all(&ephemeral).unwrap();
        std::fs::write(ephemeral.join("vo.mod"), b"partial").unwrap();

        assert_eq!(clean_cache(root.path(), None).unwrap(), 0);

        assert!(!root.path().join("ephemeral").exists());
        assert_eq!(
            std::fs::read(
                root.path()
                    .join(crate::cache::mutation_lock::CACHE_OWNER_MARKER)
            )
            .unwrap(),
            b"volang-module-cache-v2\n",
        );
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn clean_cache_removes_a_version_through_anchored_directories() {
        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let module_dir = root.path().join(crate::cache::layout::cache_key(&module));
        let version_dir = module_dir.join("1.2.3");
        std::fs::create_dir_all(version_dir.join("nested")).unwrap();
        std::fs::write(version_dir.join("nested/source.vo"), b"package source\n").unwrap();

        assert_eq!(clean_cache(root.path(), None).unwrap(), 1);
        assert!(!module_dir.exists());
    }

    #[cfg(unix)]
    #[test]
    fn clean_cache_unlinks_nested_symlinks_without_touching_their_targets() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let staging = root.path().join(crate::cache::layout::STAGING_DIR);
        let abandoned = staging.join("abandoned");
        std::fs::create_dir_all(&abandoned).unwrap();
        let outside = tempfile::tempdir().unwrap();
        let sentinel = outside.path().join("sentinel");
        std::fs::write(&sentinel, b"outside").unwrap();
        symlink(outside.path(), abandoned.join("outside-link")).unwrap();

        assert_eq!(clean_cache(root.path(), None).unwrap(), 0);
        assert!(!abandoned.exists());
        assert_eq!(std::fs::read(sentinel).unwrap(), b"outside");
    }

    #[test]
    fn clean_cache_waits_for_an_active_install_transaction() {
        use std::sync::mpsc;
        use std::time::Duration;

        let root = tempfile::tempdir().unwrap();
        let install_lock = crate::cache::acquire_read_lease(root.path()).unwrap();
        let staging = root.path().join(crate::cache::layout::STAGING_DIR);
        let active_stage = staging.join("source-active");
        std::fs::create_dir(&active_stage).unwrap();
        std::fs::write(active_stage.join("partial"), b"partial").unwrap();
        let root_path = root.path().to_path_buf();
        let (started_tx, started_rx) = mpsc::channel();
        let cleaner = std::thread::spawn(move || {
            started_tx.send(()).unwrap();
            clean_cache(&root_path, None)
        });
        started_rx.recv().unwrap();
        std::thread::sleep(Duration::from_millis(50));

        assert!(active_stage.is_dir());
        assert!(!cleaner.is_finished());
        drop(install_lock);

        assert_eq!(cleaner.join().unwrap().unwrap(), 0);
        assert!(!active_stage.exists());
    }

    #[cfg(unix)]
    #[test]
    fn clean_cache_rejects_staging_lock_symlinks_and_case_aliases() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let staging = root.path().join(crate::cache::layout::STAGING_DIR);
        std::fs::remove_file(staging.join(crate::cache::layout::STAGING_LOCK_FILE)).unwrap();
        let outside = tempfile::NamedTempFile::new().unwrap();
        symlink(
            outside.path(),
            staging.join(crate::cache::layout::STAGING_LOCK_FILE),
        )
        .unwrap();

        let error = clean_cache(root.path(), None).unwrap_err();
        assert!(error.to_string().contains("Symlink"), "{error}");
        assert_eq!(std::fs::read(outside.path()).unwrap(), b"");

        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let staging = root.path().join(crate::cache::layout::STAGING_DIR);
        std::fs::remove_file(staging.join(crate::cache::layout::STAGING_LOCK_FILE)).unwrap();
        std::fs::write(staging.join(".LOCK"), b"").unwrap();
        let error = clean_cache(root.path(), None).unwrap_err();
        assert!(error.to_string().contains("exact spelling"), "{error}");
        assert!(staging.join(".LOCK").is_file());
    }

    #[cfg(unix)]
    #[test]
    fn clean_cache_rejects_symlinked_cache_key_without_touching_target() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let outside = tempfile::tempdir().unwrap();
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let outside_version = outside.path().join("1.2.3");
        std::fs::create_dir(&outside_version).unwrap();
        let sentinel = outside_version.join("keep.vo");
        std::fs::write(&sentinel, "package keep\n").unwrap();
        symlink(
            outside.path(),
            root.path().join(crate::cache::layout::cache_key(&module)),
        )
        .unwrap();

        let error = clean_cache(root.path(), None).unwrap_err();

        assert!(error.to_string().contains("Symlink"), "{error}");
        assert_eq!(std::fs::read_to_string(sentinel).unwrap(), "package keep\n");
    }

    #[cfg(unix)]
    #[test]
    fn clean_cache_rejects_symlinked_version_without_touching_target() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let outside = tempfile::tempdir().unwrap();
        let module = ModulePath::parse("github.com/acme/lib").unwrap();
        let module_dir = root.path().join(crate::cache::layout::cache_key(&module));
        std::fs::create_dir(&module_dir).unwrap();
        let sentinel = outside.path().join("keep.vo");
        std::fs::write(&sentinel, "package keep\n").unwrap();
        symlink(outside.path(), module_dir.join("1.2.3")).unwrap();

        let error = clean_cache(root.path(), None).unwrap_err();

        assert!(error.to_string().contains("Symlink"), "{error}");
        assert_eq!(std::fs::read_to_string(sentinel).unwrap(), "package keep\n");
    }
}
