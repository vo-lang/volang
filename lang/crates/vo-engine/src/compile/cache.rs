use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ffi::OsStr;
#[cfg(unix)]
use std::fs::File;
use std::fs::{self, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use vo_common::stable_hash::StableHasher;
use vo_common::vfs::{FileSystem, RealFs};
use vo_module::ext_manifest::NativeBuildManifest;
use vo_module::project::{
    ProjectAuthority, ProjectContextOptions, ProjectDeps, SingleFileSourceGeneration,
};
use vo_module::schema::lockfile::LockedModule;
use vo_module::schema::modfile::ModFile;
use vo_runtime::ext_loader::{NativeExtensionSpec, ABI_FINGERPRINT, ABI_VERSION};
use vo_vm::bytecode::Module;

use super::host_input::{HostDirectorySnapshot, HostEntryIdentity, HostEntryKind};
use super::native::current_target_triple;
use super::snapshot::CompileInputSnapshot;
use super::{
    CompileError, CompileOutput, ProjectGraphContext, COMPILE_CACHE_NATIVE_NAMESPACE,
    COMPILE_CACHE_SCHEMA_VERSION, COMPILE_CACHE_SLOT_NAMESPACE,
};

const COMPILE_CACHE_ENTRY_NAMESPACE: &str = "vo-compile-cache-entry-v1";
const COMPILE_CACHE_PAYLOAD_NAMESPACE: &str = "vo-compile-cache-payload-v1";
const COMPILE_CACHE_ENTRY_FORMAT_VERSION: u32 = 1;
const COMPILE_CACHE_MANIFEST_MAX_BYTES: usize = 64 * 1024;
const COMPILE_CACHE_EXTENSIONS_MAX_BYTES: usize = 4 * 1024 * 1024;
const COMPILE_CACHE_LOCKED_MODULES_MAX_BYTES: usize = 16 * 1024 * 1024;
const COMPILE_INPUT_MAX_FILES_PER_TREE: usize = 100_000;
const COMPILE_INPUT_MAX_ENTRIES_PER_TREE: usize = 200_000;
const COMPILE_INPUT_MAX_DIRECTORY_DEPTH: usize = 256;
const COMPILE_CACHE_MAX_ENTRIES_PER_SLOT: usize = 4;
const COMPILE_CACHE_STALE_TEMP_AGE: std::time::Duration =
    std::time::Duration::from_secs(24 * 60 * 60);
static COMPILE_CACHE_TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(serde::Serialize, serde::Deserialize)]
struct CachedNativeExtensionSpec {
    name: String,
    module_owner: String,
    native_path: PathBuf,
    manifest_path: PathBuf,
}

impl From<&NativeExtensionSpec> for CachedNativeExtensionSpec {
    fn from(spec: &NativeExtensionSpec) -> Self {
        Self {
            name: spec.name.clone(),
            module_owner: spec.module_owner.clone(),
            native_path: spec.native_path.clone(),
            manifest_path: spec.manifest_path.clone(),
        }
    }
}

impl TryFrom<CachedNativeExtensionSpec> for NativeExtensionSpec {
    type Error = vo_common_core::extern_key::CanonicalModuleOwnerError;

    fn try_from(spec: CachedNativeExtensionSpec) -> Result<Self, Self::Error> {
        NativeExtensionSpec::try_new(
            spec.name,
            spec.module_owner,
            spec.native_path,
            spec.manifest_path,
        )
    }
}

#[derive(Debug, Clone)]
pub(super) struct CompileCacheSlot {
    pub(super) dir: PathBuf,
    #[cfg(test)]
    pub(super) fingerprint_file: PathBuf,
}

#[derive(Debug)]
struct CompileCacheEntry {
    dir: PathBuf,
    manifest_file: PathBuf,
    module_file: PathBuf,
    extensions_file: PathBuf,
    locked_modules_file: PathBuf,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct CompileCacheEntryManifest {
    format_version: u32,
    fingerprint: String,
    module_digest: String,
    extensions_digest: String,
    locked_modules_digest: String,
}

#[derive(Debug)]
pub(super) struct CapturedCompileInputs {
    fingerprint: String,
    snapshot: Arc<CompileInputSnapshot>,
}

#[derive(Clone, Copy)]
pub(super) struct CompileInputCapture<'a> {
    pub(super) source_root: &'a Path,
    pub(super) project_root: &'a Path,
    pub(super) mod_cache: &'a Path,
    pub(super) single_file: Option<&'a Path>,
    pub(super) single_file_source_generation: Option<&'a SingleFileSourceGeneration>,
    pub(super) graph: &'a ProjectGraphContext,
    pub(super) project_deps: &'a ProjectDeps,
    pub(super) workspace_sources: &'a HashMap<String, PathBuf>,
    pub(super) workspace_options: &'a ProjectContextOptions,
    pub(super) workspace_generation: &'a str,
    pub(super) stdlib_source_fingerprint: &'a str,
}

impl CapturedCompileInputs {
    pub(super) fn fingerprint(&self) -> &str {
        &self.fingerprint
    }

    pub(super) fn into_snapshot(self) -> Arc<CompileInputSnapshot> {
        self.snapshot
    }

    pub(super) fn snapshot(&self) -> Arc<CompileInputSnapshot> {
        Arc::clone(&self.snapshot)
    }
}

pub(super) fn compile_cache_slot(root: &Path, single_file: Option<&OsStr>) -> CompileCacheSlot {
    let mut slot_hasher = StableHasher::new(COMPILE_CACHE_SLOT_NAMESPACE);
    if let Some(file_name) = single_file {
        slot_hasher.update_str("entry_kind", "file");
        slot_hasher.update_path("entry_name", Path::new(file_name));
    } else {
        slot_hasher.update_str("entry_kind", "dir");
        slot_hasher.update_str("entry_name", ".");
    }
    let slot_id = slot_hasher.finish_suffix();
    let dir = repo_local_compile_cache_root(root)
        .join("compile")
        .join("native")
        .join(slot_id);
    CompileCacheSlot {
        #[cfg(test)]
        fingerprint_file: dir.join("fingerprint"),
        dir,
    }
}

fn compile_cache_entry(slot: &CompileCacheSlot, fingerprint: &str) -> CompileCacheEntry {
    let mut hasher = StableHasher::new(COMPILE_CACHE_ENTRY_NAMESPACE);
    hasher.update_str("compile_fingerprint", fingerprint);
    let dir = slot.dir.join("entries").join(hasher.finish_suffix());
    CompileCacheEntry {
        manifest_file: dir.join("manifest.json"),
        module_file: dir.join("module.voc"),
        extensions_file: dir.join("extensions.json"),
        locked_modules_file: dir.join("locked_modules.json"),
        dir,
    }
}

fn cache_entry_at(dir: PathBuf) -> CompileCacheEntry {
    CompileCacheEntry {
        manifest_file: dir.join("manifest.json"),
        module_file: dir.join("module.voc"),
        extensions_file: dir.join("extensions.json"),
        locked_modules_file: dir.join("locked_modules.json"),
        dir,
    }
}

fn repo_local_compile_cache_root(root: &Path) -> PathBuf {
    let state_root = find_volang_repo_root(root)
        .unwrap_or_else(|| root.to_path_buf())
        .join(".volang");
    state_root.join("cache").join("vo")
}

fn find_volang_repo_root(start: &Path) -> Option<PathBuf> {
    for ancestor in start.ancestors() {
        if ancestor.join("Cargo.toml").is_file() && ancestor.join("eng").is_dir() {
            return Some(ancestor.to_path_buf());
        }
    }
    None
}

pub(super) fn capture_compile_inputs(
    input: CompileInputCapture<'_>,
) -> Result<CapturedCompileInputs, CompileError> {
    capture_compile_inputs_with_between_scans(input, || {})
}

fn capture_compile_inputs_with_between_scans<F>(
    input: CompileInputCapture<'_>,
    between_scans: F,
) -> Result<CapturedCompileInputs, CompileError>
where
    F: FnOnce(),
{
    let first_fingerprint = capture_compile_inputs_once(input)
        .map_err(classify_compile_input_error)?
        .fingerprint;
    between_scans();
    let second = capture_compile_inputs_once(input).map_err(classify_compile_input_error)?;
    if first_fingerprint != second.fingerprint {
        return Err(CompileError::ModuleSystem(super::ModuleSystemError::new(
            super::ModuleSystemStage::CompileInputs,
            super::ModuleSystemErrorKind::Mismatch,
            "compile input closure changed between its two bounded captures; retry after concurrent source updates finish",
        )));
    }
    Ok(second)
}

fn classify_compile_input_error(error: CompileError) -> CompileError {
    match error {
        CompileError::Io(error) => {
            let kind = match error.kind() {
                io::ErrorKind::NotFound => super::ModuleSystemErrorKind::Missing,
                io::ErrorKind::InvalidData
                | io::ErrorKind::InvalidInput
                | io::ErrorKind::Unsupported => super::ModuleSystemErrorKind::ValidationFailed,
                _ => super::ModuleSystemErrorKind::ReadFailed,
            };
            CompileError::ModuleSystem(super::ModuleSystemError::new(
                super::ModuleSystemStage::CompileInputs,
                kind,
                format!("failed to capture immutable compile inputs: {error}"),
            ))
        }
        error => error,
    }
}

fn capture_compile_inputs_once(
    input: CompileInputCapture<'_>,
) -> Result<CapturedCompileInputs, CompileError> {
    let CompileInputCapture {
        source_root,
        project_root,
        mod_cache,
        single_file,
        single_file_source_generation,
        graph,
        project_deps,
        workspace_sources,
        workspace_options,
        workspace_generation,
        stdlib_source_fingerprint,
    } = input;
    if let Some(source_generation) = single_file_source_generation {
        validate_live_single_file_generation(source_generation)?;
    }
    let canonical_source_root = source_root
        .canonicalize()
        .unwrap_or_else(|_| source_root.to_path_buf());
    let canonical_project_root = project_root
        .canonicalize()
        .unwrap_or_else(|_| project_root.to_path_buf());
    let canonical_mod_cache = mod_cache
        .canonicalize()
        .unwrap_or_else(|_| mod_cache.to_path_buf());
    let mut snapshot = CompileInputSnapshot::default();
    let mut hasher = StableHasher::new(COMPILE_CACHE_NATIVE_NAMESPACE);
    hasher.update_str("schema", COMPILE_CACHE_SCHEMA_VERSION);
    hasher.update_str("compiler_version", vo_module::TOOLCHAIN_VERSION);
    hasher.update_str("compiler_build_id", env!("VO_COMPILER_BUILD_ID"));
    hasher.update_str("stdlib_source_fingerprint", stdlib_source_fingerprint);
    hasher.update_str("target_triple", current_target_triple());
    hasher.update_str("extension_abi_version", &ABI_VERSION.to_string());
    hasher.update_str(
        "extension_abi_fingerprint",
        &format!("{ABI_FINGERPRINT:#x}"),
    );
    hasher.update_str("workspace_generation", workspace_generation);
    hasher.update_path("source_root", &canonical_source_root);
    hasher.update_path("project_root", &canonical_project_root);
    hasher.update_path("mod_cache_root", &canonical_mod_cache);
    hasher.update_bool("single_file", single_file.is_some());
    if let Some(file_path) = single_file {
        hasher.update_path("entry_file", file_path);
    } else {
        hasher.update_str("entry_file", ".");
    }
    hash_project_graph_context(&mut hasher, graph)?;

    // This base snapshot freezes language sources and module/workspace
    // authority. Native adapter inputs stay outside it on purpose: after
    // analysis identifies the reached extension owners, native preparation
    // captures and authenticates each complete Cargo or prebuilt generation.
    // Cache hits repeat that per-reached-extension validation before accepting
    // retained immutable load-copy paths.
    let excluded_module_tree_roots = BTreeSet::from([canonical_mod_cache.clone()]);
    let mut native_module_dirs = capture_compile_input_tree_with_exclusions(
        &mut hasher,
        &mut snapshot,
        "project_root",
        &canonical_project_root,
        &excluded_module_tree_roots,
    )?;
    capture_project_graph_inputs(&mut hasher, &mut snapshot, graph)?;
    hash_project_deps(&mut hasher, project_deps);

    let workspace_file = vo_module::workspace::discover_workfile_in_with(
        &RealFs::new("."),
        &canonical_project_root,
        &workspace_options.workspace,
    )
    .map_err(workspace_capture_error)?;
    if let Some(workfile_path) = workspace_file {
        let bytes = snapshot.capture_file(&workfile_path)?;
        hasher.update_path("workspace_file", &workfile_path);
        hasher.update_bytes("workspace_file_bytes", bytes);
    } else {
        hasher.update_str("workspace_file", "");
    }

    let mut workspace_source_entries = workspace_sources
        .iter()
        .map(|(module, path)| {
            let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
            (module.clone(), canonical)
        })
        .collect::<Vec<_>>();
    workspace_source_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut seen_roots = BTreeSet::new();
    for (module, workspace_source_root) in workspace_source_entries {
        hasher.update_str("workspace_source_module", &module);
        hasher.update_path("workspace_source_root", &workspace_source_root);
        if seen_roots.insert(workspace_source_root.clone()) {
            native_module_dirs.extend(capture_compile_input_tree_with_exclusions(
                &mut hasher,
                &mut snapshot,
                &format!("workspace-source:{module}"),
                &workspace_source_root,
                &excluded_module_tree_roots,
            )?);
        }
    }

    capture_locked_module_inputs(
        &mut hasher,
        &mut snapshot,
        &canonical_mod_cache,
        project_deps,
    )?;

    for (module_dir, cargo_manifest) in native_module_dirs {
        hasher.update_path("native_module_dir", &module_dir);
        hasher.update_path("native_cargo_manifest", &cargo_manifest);
    }

    if let Some(source_generation) = single_file_source_generation {
        let source_path = source_generation.path();
        let source = snapshot.read_file(source_path).map_err(|error| {
            CompileError::ModuleSystem(
                super::ModuleSystemError::new(
                    super::ModuleSystemStage::CompileInputs,
                    super::ModuleSystemErrorKind::ReadFailed,
                    format!(
                        "captured single-file source at {} could not be validated against its classification generation: {error}",
                        source_path.display(),
                    ),
                )
                .with_path(source_path),
            )
        })?;
        if !source_generation.matches_source(source_path, &source) {
            return Err(CompileError::ModuleSystem(
                super::ModuleSystemError::new(
                    super::ModuleSystemStage::CompileInputs,
                    super::ModuleSystemErrorKind::Mismatch,
                    "single-file source changed after module classification; retry so classification and compilation use one generation",
                )
                .with_path(source_path),
            ));
        }
        validate_live_single_file_generation(source_generation)?;
        hasher.update_str(
            "single_file_classification_generation",
            source_generation.generation_key(),
        );
    } else {
        hasher.update_str("single_file_classification_generation", "");
    }

    Ok(CapturedCompileInputs {
        fingerprint: hasher.finish(),
        snapshot: Arc::new(snapshot),
    })
}

fn validate_live_single_file_generation(
    source_generation: &SingleFileSourceGeneration,
) -> Result<(), CompileError> {
    source_generation
        .validate(&RealFs::new("."))
        .map_err(|error| {
            CompileError::ModuleSystem(
                super::ModuleSystemError::new(
                    super::ModuleSystemStage::CompileInputs,
                    super::ModuleSystemErrorKind::Mismatch,
                    format!("single-file project authority changed after classification: {error}"),
                )
                .with_path(source_generation.path()),
            )
        })
}

fn hash_project_graph_context(
    hasher: &mut StableHasher,
    graph: &ProjectGraphContext,
) -> Result<(), CompileError> {
    hasher.update_str(
        "project_authority",
        match graph.authority {
            ProjectAuthority::Empty => "empty",
            ProjectAuthority::Lock => "lock",
            ProjectAuthority::Workspace => "workspace",
        },
    );
    hasher.update_str(
        "project_metadata_generation",
        &graph.project_metadata_generation,
    );
    let mut modules = graph.workspace_modules.iter().collect::<Vec<_>>();
    modules.sort_by(|left, right| {
        left.module()
            .as_str()
            .cmp(right.module().as_str())
            .then_with(|| left.directory().cmp(right.directory()))
    });
    hasher.update_bytes(
        "workspace_authority_module_count",
        &u64::try_from(modules.len())
            .unwrap_or(u64::MAX)
            .to_le_bytes(),
    );
    for module in modules {
        let declaration = module.mod_file().render().map_err(|error| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "authorized workspace manifest for {} cannot be rendered: {error}",
                    module.module(),
                ),
            )
        })?;
        hasher.update_str("workspace_authority_module", module.module().as_str());
        hasher.update_path(
            "workspace_authority_directory",
            &module
                .directory()
                .canonicalize()
                .unwrap_or_else(|_| module.directory().to_path_buf()),
        );
        hasher.update_str("workspace_authority_manifest", &declaration);
    }
    Ok(())
}

fn capture_project_graph_inputs(
    hasher: &mut StableHasher,
    snapshot: &mut CompileInputSnapshot,
    graph: &ProjectGraphContext,
) -> Result<(), CompileError> {
    if graph.validated_input_files.len() > COMPILE_INPUT_MAX_FILES_PER_TREE {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "project authority exceeds the {}-file validation-input limit",
                COMPILE_INPUT_MAX_FILES_PER_TREE,
            ),
        )
        .into());
    }
    let mut paths = graph.validated_input_files.clone();
    paths.sort();
    paths.dedup();
    hasher.update_bytes(
        "project_authority_input_count",
        &u64::try_from(paths.len()).unwrap_or(u64::MAX).to_le_bytes(),
    );
    for path in paths {
        let bytes = snapshot.capture_file(&path)?;
        hasher.update_path("project_authority_input", &path);
        hasher.update_bytes("project_authority_input_bytes", bytes);
    }
    Ok(())
}

fn workspace_capture_error(error: vo_module::Error) -> CompileError {
    let kind = match error {
        vo_module::Error::Io(_) => super::ModuleSystemErrorKind::ReadFailed,
        vo_module::Error::WorkFileParse(_) => super::ModuleSystemErrorKind::ParseFailed,
        _ => super::ModuleSystemErrorKind::ValidationFailed,
    };
    CompileError::ModuleSystem(super::ModuleSystemError::new(
        super::ModuleSystemStage::Workspace,
        kind,
        format!("workspace discovery failed while capturing compile inputs: {error}"),
    ))
}

fn hash_project_deps(hasher: &mut StableHasher, project_deps: &ProjectDeps) {
    hasher.update_bool("project_deps_has_mod_file", project_deps.has_mod_file());
    hasher.update_str(
        "project_deps_current_module",
        project_deps.current_module().unwrap_or(""),
    );
    hasher.update_str(
        "project_deps_mod_file",
        &project_deps
            .mod_file()
            .map(|mod_file| {
                mod_file
                    .render()
                    .expect("loaded vo.mod must satisfy its schema")
            })
            .unwrap_or_default(),
    );
    hasher.update_str(
        "project_deps_lock_file",
        &project_deps
            .lock_file()
            .map(|lock_file| {
                lock_file
                    .render()
                    .expect("loaded vo.lock must satisfy its schema")
            })
            .unwrap_or_default(),
    );
}

#[cfg(test)]
fn capture_compile_input_tree(
    hasher: &mut StableHasher,
    snapshot: &mut CompileInputSnapshot,
    label: &str,
    root: &Path,
) -> Result<BTreeMap<PathBuf, PathBuf>, CompileError> {
    capture_compile_input_tree_with_exclusions(hasher, snapshot, label, root, &BTreeSet::new())
}

fn capture_compile_input_tree_with_exclusions(
    hasher: &mut StableHasher,
    snapshot: &mut CompileInputSnapshot,
    label: &str,
    root: &Path,
    excluded_directories: &BTreeSet<PathBuf>,
) -> Result<BTreeMap<PathBuf, PathBuf>, CompileError> {
    let canonical_root = root.canonicalize()?;
    let root = canonical_root.as_path();
    let root_directory =
        super::host_input::read_stable_directory(root, COMPILE_INPUT_MAX_ENTRIES_PER_TREE)?;
    let discovery =
        discover_native_module_inputs(root, root_directory.clone(), excluded_directories)?;
    let NativeModuleInputDiscovery {
        manifest_digests,
        native_module_roots,
        reserved_native_roots,
        deferred_native_files,
    } = discovery;
    let mut files = Vec::<CapturedTreeFile>::new();
    // The collector already selects only Vo sources and module control files.
    // A declared Cargo manifest may live beside a real Vo package, so its
    // parent directory must remain visible to the language snapshot.
    let mut language_tree_exclusions = excluded_directories.clone();
    language_tree_exclusions.extend(reserved_native_roots);
    collect_compile_input_files_matching_with_options(
        root,
        root,
        root_directory.clone(),
        &mut files,
        CompileInputLimits {
            entries: COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
            files: snapshot.remaining_files(),
            bytes: snapshot.remaining_bytes(),
        },
        CompileInputFileSelection {
            include_file: is_module_compile_input_file,
            tree_kind: CompileInputTreeKind::ModuleTree,
            excluded_directories: &language_tree_exclusions,
            excluded_files: &deferred_native_files,
        },
    )?;
    let captured_manifests = files
        .iter()
        .filter(|file| {
            file.relative
                .file_name()
                .is_some_and(|name| name == "vo.mod")
        })
        .map(|file| file.relative.clone())
        .collect::<BTreeSet<_>>();
    let discovered_manifests = manifest_digests.keys().cloned().collect::<BTreeSet<_>>();
    if captured_manifests != discovered_manifests {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "compile input manifests changed while scanning {}",
                root.display(),
            ),
        )
        .into());
    }
    for rel in &captured_manifests {
        let bytes = files
            .iter()
            .find(|file| &file.relative == rel)
            .map(|file| file.bytes.as_slice())
            .expect("captured manifest path must retain its bytes");
        if manifest_digests
            .get(rel)
            .is_none_or(|discovered| discovered != &compile_input_manifest_digest(bytes))
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "compile input manifest changed while scanning {}",
                    root.join(rel).display(),
                ),
            )
            .into());
        }
    }

    files.sort_by(|left, right| left.relative.cmp(&right.relative));
    files.dedup_by(|left, right| left.relative == right.relative && left.bytes == right.bytes);
    if files.len() > COMPILE_INPUT_MAX_FILES_PER_TREE {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "compile input tree at {} exceeds the {}-file limit",
                root.display(),
                COMPILE_INPUT_MAX_FILES_PER_TREE,
            ),
        )
        .into());
    }

    hasher.update_str("tree_label", label);
    hasher.update_path("tree_root", root);
    for file in files {
        let path = root.join(&file.relative);
        let bytes = snapshot.insert_consistent(path, file.bytes)?;
        hasher.update_path("file_path", &file.relative);
        hasher.update_bytes("file_bytes", bytes);
    }

    Ok(native_module_roots)
}

#[derive(Default)]
struct NativeModuleInputDiscovery {
    manifest_digests: BTreeMap<PathBuf, String>,
    native_module_roots: BTreeMap<PathBuf, PathBuf>,
    reserved_native_roots: BTreeSet<PathBuf>,
    deferred_native_files: BTreeSet<PathBuf>,
}

fn is_deferred_native_path_entry(
    path: &Path,
    kind: HostEntryKind,
    deferred_files: &BTreeSet<PathBuf>,
) -> io::Result<bool> {
    if portable_path_set_contains(deferred_files, path)? {
        return Ok(true);
    }
    if !matches!(kind, HostEntryKind::Symlink | HostEntryKind::Special) {
        return Ok(false);
    }
    for deferred in deferred_files {
        if super::host_input::portable_host_path_starts_with(deferred, path)? {
            return Ok(true);
        }
    }
    Ok(false)
}

#[derive(Debug)]
struct CapturedTreeFile {
    relative: PathBuf,
    bytes: Vec<u8>,
}

fn compile_input_capture_limit_error(root: &Path) -> CompileError {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "compile input closure at {} exceeds the global {}-file or {}-byte snapshot limit",
            root.display(),
            super::snapshot::MAX_COMPILE_SNAPSHOT_FILES,
            super::snapshot::MAX_COMPILE_SNAPSHOT_BYTES,
        ),
    )
    .into()
}

fn compile_input_manifest_digest(bytes: &[u8]) -> String {
    let mut hasher = StableHasher::new("vo-compile-input-manifest-v1");
    hasher.update_bytes("manifest", bytes);
    hasher.finish()
}

fn discover_native_module_inputs(
    root: &Path,
    root_directory: HostDirectorySnapshot,
    excluded_directories: &BTreeSet<PathBuf>,
) -> Result<NativeModuleInputDiscovery, CompileError> {
    let mut discovery = NativeModuleInputDiscovery::default();
    let mut walk = CompileInputWalkState::default();
    discover_native_module_inputs_inner(
        root,
        root,
        root_directory,
        excluded_directories,
        &mut discovery,
        &mut walk,
        0,
    )?;
    Ok(discovery)
}

fn discover_native_module_inputs_inner(
    root: &Path,
    dir: &Path,
    directory: HostDirectorySnapshot,
    excluded_directories: &BTreeSet<PathBuf>,
    discovery: &mut NativeModuleInputDiscovery,
    walk: &mut CompileInputWalkState,
    depth: usize,
) -> Result<(), CompileError> {
    let directory = enter_compile_input_directory(walk, dir, depth, "compile input", directory)?;
    let directory_identity = directory.identity.clone();
    let directory_capability = directory.capability.clone();
    let result = (|| {
        let entries = directory.entries;
        consume_compile_input_entries(walk, entries.len(), root, "compile input")?;

        if let Some(entry) = entries
            .iter()
            .find(|entry| entry.name == OsStr::new("vo.mod"))
        {
            let path = dir.join(&entry.name);
            match entry.kind {
                HostEntryKind::RegularFile => {
                    let remaining_bytes = walk.byte_limit.saturating_sub(walk.bytes);
                    let bytes = super::host_input::read_stable_regular_child(
                        &directory_capability,
                        &entry.name,
                        &path,
                        vo_common::vfs::MAX_TEXT_FILE_BYTES.min(remaining_bytes),
                    )?;
                    consume_compile_input_file(walk, bytes.len(), root, "compile input discovery")?;
                    let relative = path.strip_prefix(root).unwrap_or(&path).to_path_buf();
                    discovery
                        .manifest_digests
                        .insert(relative, compile_input_manifest_digest(&bytes));
                    discover_native_module_manifest(&path, &bytes, discovery)?;
                }
                HostEntryKind::Symlink => {
                    return Err(compile_input_symlink_error(&path, "compile input").into());
                }
                HostEntryKind::Directory | HostEntryKind::Special => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("compile input at {} is not a regular file", path.display()),
                    )
                    .into());
                }
            }
        }

        for entry in entries {
            let path = dir.join(&entry.name);
            if is_deferred_native_path_entry(&path, entry.kind, &discovery.deferred_native_files)? {
                continue;
            }
            if portable_path_set_contains(&discovery.reserved_native_roots, &path)?
                || should_skip_compile_input_dir_with_options(
                    &path,
                    CompileInputTreeKind::ModuleTree,
                    excluded_directories,
                )?
            {
                continue;
            }
            match entry.kind {
                HostEntryKind::Directory => {
                    let child = super::host_input::read_stable_directory_child(
                        &directory_capability,
                        &entry.name,
                        &path,
                        walk.entry_limit.saturating_sub(walk.entries),
                    )?;
                    if is_declared_cache_directory(&path, &child)? {
                        continue;
                    }
                    discover_native_module_inputs_inner(
                        root,
                        &path,
                        child,
                        excluded_directories,
                        discovery,
                        walk,
                        depth.saturating_add(1),
                    )?;
                }
                HostEntryKind::Symlink => {
                    return Err(compile_input_symlink_error(&path, "compile input").into());
                }
                HostEntryKind::Special => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "compile input at {} is a special filesystem entry",
                            path.display()
                        ),
                    )
                    .into());
                }
                HostEntryKind::RegularFile => {}
            }
        }
        Ok(())
    })();
    walk.ancestors.remove(&directory_identity);
    result
}

fn discover_native_module_manifest(
    manifest_path: &Path,
    bytes: &[u8],
    discovery: &mut NativeModuleInputDiscovery,
) -> Result<(), CompileError> {
    let Some(module_dir) = manifest_path.parent() else {
        return Ok(());
    };
    let content = std::str::from_utf8(bytes).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "compile input manifest at {} is not valid UTF-8: {error}",
                manifest_path.display(),
            ),
        )
    })?;
    let extension = ModFile::parse_project_at(content, manifest_path)
        .map_err(|error| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "invalid compile input manifest at {}: {error}",
                    manifest_path.display(),
                ),
            )
        })?
        .extension;
    let Some(extension) = extension else {
        return Ok(());
    };
    match extension.native_build() {
        Some(NativeBuildManifest::Cargo { manifest, .. }) => {
            let cargo_manifest = module_dir.join(manifest);
            let native_root = extension
                .resolve_native_cargo_root_path(module_dir)
                .map_err(|error| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "invalid compile input manifest at {}: {error}",
                            manifest_path.display(),
                        ),
                    )
                })?
                .expect("Cargo build manifest must resolve one reserved native root");
            discovery.reserved_native_roots.insert(native_root);
            if extension.native.is_some() && extension.supports_target(current_target_triple()) {
                discovery
                    .native_module_roots
                    .insert(module_dir.to_path_buf(), cargo_manifest);
            }
        }
        Some(NativeBuildManifest::Prebuilt { path }) => {
            discovery
                .deferred_native_files
                .insert(module_dir.join(path));
        }
        None => {}
    }
    Ok(())
}

fn is_declared_cache_directory(
    directory_path: &Path,
    directory: &HostDirectorySnapshot,
) -> Result<bool, CompileError> {
    let Some(tag) = directory
        .entries
        .iter()
        .find(|entry| entry.name == OsStr::new("CACHEDIR.TAG"))
    else {
        return Ok(false);
    };
    if tag.kind != HostEntryKind::RegularFile {
        return Ok(false);
    }
    let tag_path = directory_path.join(&tag.name);
    let bytes = super::host_input::read_stable_regular_child(
        &directory.capability,
        &tag.name,
        &tag_path,
        1024,
    )?;
    Ok(super::host_input::cache_directory_tag_has_signature(&bytes))
}

fn capture_locked_module_inputs(
    hasher: &mut StableHasher,
    snapshot: &mut CompileInputSnapshot,
    mod_cache: &Path,
    project_deps: &ProjectDeps,
) -> Result<(), CompileError> {
    let mut modules = project_deps.locked_modules().iter().collect::<Vec<_>>();
    modules.sort_by(|left, right| {
        (left.path.as_str(), left.version.to_string())
            .cmp(&(right.path.as_str(), right.version.to_string()))
    });

    for locked in modules {
        let relative_root =
            vo_module::cache::layout::relative_module_dir(&locked.path, &locked.version);
        let root = mod_cache.join(&relative_root);
        let root_directory =
            super::host_input::read_stable_directory(&root, COMPILE_INPUT_MAX_ENTRIES_PER_TREE)?;
        let release_path = root.join("vo.release.json");
        let artifact_paths = {
            if snapshot.remaining_files() == 0 {
                return Err(compile_input_capture_limit_error(&root));
            }
            let release_bytes = super::host_input::read_stable_descendant_file(
                &root_directory.capability,
                Path::new("vo.release.json"),
                &release_path,
                super::snapshot::compile_input_limit(&release_path).min(snapshot.remaining_bytes()),
            )?;
            let artifact_paths = locked_module_artifact_input_paths(locked, &release_bytes)?;
            snapshot.insert_consistent(release_path, release_bytes)?;
            artifact_paths
        };
        let mut files = Vec::new();
        collect_locked_module_input_files(
            &root,
            &root,
            root_directory,
            &artifact_paths,
            &mut files,
            snapshot.remaining_files(),
            snapshot.remaining_bytes(),
        )?;
        files.sort_by(|left, right| left.relative.cmp(&right.relative));

        hasher.update_str("locked_module", locked.path.as_str());
        hasher.update_str("locked_version", &locked.version.to_string());
        hasher.update_path("locked_module_root", &root);
        for file in files {
            let path = root.join(&file.relative);
            let bytes = snapshot.insert_consistent(path, file.bytes)?;
            hasher.update_path("locked_file_path", &file.relative);
            hasher.update_bytes("locked_file_bytes", bytes);
        }
    }
    Ok(())
}

fn locked_module_artifact_input_paths(
    locked: &LockedModule,
    release_bytes: &[u8],
) -> Result<BTreeSet<PathBuf>, CompileError> {
    let release_digest = vo_module::digest::Digest::from_sha256(release_bytes);
    if release_digest != locked.release {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "cached vo.release.json for {}@{} does not match locked digest: expected {}, found {}",
                locked.path, locked.version, locked.release, release_digest,
            ),
        )
        .into());
    }
    let release_content = std::str::from_utf8(release_bytes).map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "cached vo.release.json for {}@{} is not valid UTF-8: {error}",
                locked.path, locked.version,
            ),
        )
    })?;
    let release = vo_module::registry::parse_requested_release_manifest(
        release_content,
        &locked.path,
        &locked.version,
    )
    .map_err(|error| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "invalid cached vo.release.json for {}@{}: {error}",
                locked.path, locked.version,
            ),
        )
    })?;
    vo_module::lock::validate_locked_module_against_manifest(locked, &release, &release_digest)
        .map_err(|error| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "cached vo.release.json for {}@{} does not match vo.lock: {error}",
                    locked.path, locked.version,
                ),
            )
        })?;

    release
        .artifacts
        .iter()
        .filter(|artifact| artifact.id.target == current_target_triple())
        .map(|artifact| {
            vo_module::artifact::artifact_relative_path(&artifact.id).map_err(|error| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("invalid locked artifact {}: {error}", artifact.id),
                )
                .into()
            })
        })
        .collect()
}

fn collect_locked_module_input_files(
    root: &Path,
    dir: &Path,
    directory: HostDirectorySnapshot,
    artifact_paths: &BTreeSet<PathBuf>,
    out: &mut Vec<CapturedTreeFile>,
    file_limit: usize,
    byte_limit: usize,
) -> Result<(), CompileError> {
    let mut walk = CompileInputWalkState::with_limits(
        COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
        file_limit,
        byte_limit,
    );
    collect_locked_module_input_files_inner(root, dir, directory, artifact_paths, out, &mut walk, 0)
}

fn collect_locked_module_input_files_inner(
    root: &Path,
    dir: &Path,
    directory: HostDirectorySnapshot,
    artifact_paths: &BTreeSet<PathBuf>,
    out: &mut Vec<CapturedTreeFile>,
    walk: &mut CompileInputWalkState,
    depth: usize,
) -> Result<(), CompileError> {
    let directory = enter_compile_input_directory(walk, dir, depth, "locked module", directory)?;
    let directory_identity = directory.identity.clone();
    let directory_capability = directory.capability.clone();
    let result = (|| {
        let entries = directory.entries;
        consume_compile_input_entries(walk, entries.len(), root, "locked module")?;
        for entry in entries {
            let path = dir.join(&entry.name);
            if should_skip_compile_input_dir(&path)? {
                continue;
            }
            match entry.kind {
                HostEntryKind::Symlink => {
                    return Err(compile_input_symlink_error(&path, "locked module").into());
                }
                HostEntryKind::Special => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "locked module input at {} is a special filesystem entry",
                            path.display()
                        ),
                    )
                    .into());
                }
                HostEntryKind::Directory => {
                    collect_locked_module_input_files_inner(
                        root,
                        &path,
                        super::host_input::read_stable_directory_child(
                            &directory_capability,
                            &entry.name,
                            &path,
                            walk.entry_limit.saturating_sub(walk.entries),
                        )?,
                        artifact_paths,
                        out,
                        walk,
                        depth.saturating_add(1),
                    )?;
                    continue;
                }
                HostEntryKind::RegularFile => {}
            }

            let rel = path.strip_prefix(root).unwrap_or(&path).to_path_buf();
            let root_file_name = rel.file_name().and_then(|name| name.to_str());
            let is_root_file = rel
                .parent()
                .is_none_or(|parent| parent.as_os_str().is_empty());
            let root_metadata = is_root_file
                && (matches!(
                    root_file_name,
                    Some("vo.mod") | Some("vo.release.json") | Some("vo.package.json")
                ) || root_file_name == Some(vo_module::cache::layout::VERSION_MARKER)
                    || root_file_name == Some(vo_module::cache::layout::SOURCE_DIGEST_MARKER));
            if path.extension().is_some_and(|extension| extension == "vo")
                || root_metadata
                || artifact_paths.contains(&rel)
            {
                if walk.files >= walk.file_limit {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "locked module input tree at {} exceeds its remaining {}-file snapshot budget",
                            root.display(),
                            walk.file_limit,
                        ),
                    )
                    .into());
                }
                let remaining_bytes = walk.byte_limit.saturating_sub(walk.bytes);
                let bytes = super::host_input::read_stable_regular_child(
                    &directory_capability,
                    &entry.name,
                    &path,
                    super::snapshot::compile_input_limit(&path).min(remaining_bytes),
                )?;
                consume_compile_input_file(walk, bytes.len(), root, "locked module")?;
                out.push(CapturedTreeFile {
                    relative: rel,
                    bytes,
                });
            }
        }
        Ok(())
    })();
    walk.ancestors.remove(&directory_identity);
    result
}

#[cfg(test)]
fn collect_compile_input_files(
    root: &Path,
    dir: &Path,
    out: &mut Vec<PathBuf>,
) -> Result<(), CompileError> {
    let directory =
        super::host_input::read_stable_directory(dir, COMPILE_INPUT_MAX_ENTRIES_PER_TREE)?;
    let mut captured = Vec::new();
    collect_compile_input_files_matching(
        root,
        dir,
        directory,
        &mut captured,
        is_module_compile_input_file,
        COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
    )
    .map(|_| out.extend(captured.into_iter().map(|file| file.relative)))
}

pub(super) fn collect_module_compile_input_files(
    root: &Path,
    entry_limit: usize,
    file_limit: usize,
    byte_limit: usize,
) -> Result<(CapturedInputFiles, usize), CompileError> {
    let mut files = Vec::new();
    let directory = super::host_input::read_stable_directory(root, entry_limit)?;
    let entries = collect_compile_input_files_matching_with_options(
        root,
        root,
        directory,
        &mut files,
        CompileInputLimits {
            entries: entry_limit,
            files: file_limit,
            bytes: byte_limit,
        },
        CompileInputFileSelection {
            include_file: is_module_compile_input_file,
            tree_kind: CompileInputTreeKind::ModuleTree,
            excluded_directories: &BTreeSet::new(),
            excluded_files: &BTreeSet::new(),
        },
    )?;
    files.sort_by(|left, right| left.relative.cmp(&right.relative));
    Ok((
        files
            .into_iter()
            .map(|file| (file.relative, file.bytes))
            .collect(),
        entries,
    ))
}

#[cfg(test)]
fn collect_compile_input_files_matching(
    root: &Path,
    dir: &Path,
    directory: HostDirectorySnapshot,
    out: &mut Vec<CapturedTreeFile>,
    include_file: fn(&Path, &Path) -> bool,
    entry_limit: usize,
) -> Result<usize, CompileError> {
    collect_compile_input_files_matching_with_options(
        root,
        dir,
        directory,
        out,
        CompileInputLimits {
            entries: entry_limit,
            files: super::snapshot::MAX_COMPILE_SNAPSHOT_FILES,
            bytes: super::snapshot::MAX_COMPILE_SNAPSHOT_BYTES,
        },
        CompileInputFileSelection {
            include_file,
            tree_kind: CompileInputTreeKind::ModuleTree,
            excluded_directories: &BTreeSet::new(),
            excluded_files: &BTreeSet::new(),
        },
    )
}

fn collect_compile_input_files_matching_with_options(
    root: &Path,
    dir: &Path,
    directory: HostDirectorySnapshot,
    out: &mut Vec<CapturedTreeFile>,
    limits: CompileInputLimits,
    selection: CompileInputFileSelection<'_>,
) -> Result<usize, CompileError> {
    let excluded_directories = selection.excluded_directories;
    let mut effective_excluded_directories = excluded_directories.clone();
    let excluded_files = selection.excluded_files;
    let mut effective_excluded_files = excluded_files.clone();
    if let Ok(canonical_root) = root.canonicalize() {
        for excluded in excluded_directories {
            if let Ok(relative) = excluded.strip_prefix(&canonical_root) {
                effective_excluded_directories.insert(root.join(relative));
            }
        }
        for excluded in excluded_files {
            if let Ok(relative) = excluded.strip_prefix(&canonical_root) {
                effective_excluded_files.insert(root.join(relative));
            }
        }
    }
    let mut walk = CompileInputWalkState::with_limits(limits.entries, limits.files, limits.bytes);
    collect_compile_input_files_inner(
        root,
        dir,
        directory,
        out,
        &mut walk,
        0,
        CompileInputFileSelection {
            excluded_directories: &effective_excluded_directories,
            excluded_files: &effective_excluded_files,
            ..selection
        },
    )?;
    Ok(walk.entries)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompileInputTreeKind {
    ModuleTree,
}

#[derive(Clone, Copy)]
struct CompileInputLimits {
    entries: usize,
    files: usize,
    bytes: usize,
}

#[derive(Clone, Copy)]
struct CompileInputFileSelection<'a> {
    include_file: fn(&Path, &Path) -> bool,
    tree_kind: CompileInputTreeKind,
    excluded_directories: &'a BTreeSet<PathBuf>,
    excluded_files: &'a BTreeSet<PathBuf>,
}

type CapturedInputFiles = Vec<(PathBuf, Vec<u8>)>;

fn collect_compile_input_files_inner(
    root: &Path,
    dir: &Path,
    directory: HostDirectorySnapshot,
    out: &mut Vec<CapturedTreeFile>,
    walk: &mut CompileInputWalkState,
    depth: usize,
    selection: CompileInputFileSelection<'_>,
) -> Result<(), CompileError> {
    let directory = enter_compile_input_directory(walk, dir, depth, "compile input", directory)?;
    let directory_identity = directory.identity.clone();
    let directory_capability = directory.capability.clone();
    let result = (|| {
        let entries = directory.entries;
        consume_compile_input_entries(walk, entries.len(), root, "compile input")?;
        for entry in entries {
            let path = dir.join(&entry.name);
            if is_deferred_native_path_entry(&path, entry.kind, selection.excluded_files)? {
                continue;
            }
            if should_skip_compile_input_dir_with_options(
                &path,
                selection.tree_kind,
                selection.excluded_directories,
            )? {
                continue;
            }
            match entry.kind {
                HostEntryKind::Symlink => {
                    return Err(compile_input_symlink_error(&path, "compile input").into());
                }
                HostEntryKind::Special => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "compile input at {} is a special filesystem entry",
                            path.display()
                        ),
                    )
                    .into());
                }
                HostEntryKind::Directory => {
                    let child = super::host_input::read_stable_directory_child(
                        &directory_capability,
                        &entry.name,
                        &path,
                        walk.entry_limit.saturating_sub(walk.entries),
                    )?;
                    if is_declared_cache_directory(&path, &child)? {
                        continue;
                    }
                    collect_compile_input_files_inner(
                        root,
                        &path,
                        child,
                        out,
                        walk,
                        depth.saturating_add(1),
                        selection,
                    )?;
                    continue;
                }
                HostEntryKind::RegularFile => {}
            }
            if !(selection.include_file)(root, &path) {
                continue;
            }
            if walk.files >= walk.file_limit {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "compile input tree at {} exceeds its remaining {}-file snapshot budget",
                        root.display(),
                        walk.file_limit,
                    ),
                )
                .into());
            }
            let remaining_bytes = walk.byte_limit.saturating_sub(walk.bytes);
            let bytes = super::host_input::read_stable_regular_child(
                &directory_capability,
                &entry.name,
                &path,
                super::snapshot::compile_input_limit(&path).min(remaining_bytes),
            )?;
            consume_compile_input_file(walk, bytes.len(), root, "compile input")?;
            out.push(CapturedTreeFile {
                relative: path.strip_prefix(root).unwrap_or(&path).to_path_buf(),
                bytes,
            });
        }
        Ok(())
    })();
    walk.ancestors.remove(&directory_identity);
    result
}

struct CompileInputWalkState {
    entries: usize,
    entry_limit: usize,
    files: usize,
    file_limit: usize,
    bytes: usize,
    byte_limit: usize,
    ancestors: BTreeSet<HostEntryIdentity>,
}

impl CompileInputWalkState {
    fn with_entry_limit(entry_limit: usize) -> Self {
        Self::with_limits(
            entry_limit,
            super::snapshot::MAX_COMPILE_SNAPSHOT_FILES,
            super::snapshot::MAX_COMPILE_SNAPSHOT_BYTES,
        )
    }

    fn with_limits(entry_limit: usize, file_limit: usize, byte_limit: usize) -> Self {
        Self {
            entries: 0,
            entry_limit,
            files: 0,
            file_limit,
            bytes: 0,
            byte_limit,
            ancestors: BTreeSet::new(),
        }
    }
}

impl Default for CompileInputWalkState {
    fn default() -> Self {
        Self::with_entry_limit(COMPILE_INPUT_MAX_ENTRIES_PER_TREE)
    }
}

fn enter_compile_input_directory(
    walk: &mut CompileInputWalkState,
    dir: &Path,
    depth: usize,
    description: &str,
    directory: HostDirectorySnapshot,
) -> Result<HostDirectorySnapshot, CompileError> {
    if depth > COMPILE_INPUT_MAX_DIRECTORY_DEPTH {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "{description} directory tree at {} exceeds the maximum depth of {}",
                dir.display(),
                COMPILE_INPUT_MAX_DIRECTORY_DEPTH,
            ),
        )
        .into());
    }
    if directory.entries.len() > walk.entry_limit.saturating_sub(walk.entries) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "{description} tree at {} exceeds the {}-entry limit",
                dir.display(),
                walk.entry_limit
            ),
        )
        .into());
    }
    if !walk.ancestors.insert(directory.identity.clone()) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "{description} directory cycle or identity alias detected at {}",
                dir.display(),
            ),
        )
        .into());
    }
    Ok(directory)
}

fn consume_compile_input_entries(
    walk: &mut CompileInputWalkState,
    count: usize,
    root: &Path,
    description: &str,
) -> Result<(), CompileError> {
    walk.entries = walk.entries.checked_add(count).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("{description} entry count overflow at {}", root.display()),
        )
    })?;
    if walk.entries > walk.entry_limit {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "{description} tree at {} exceeds the {}-entry limit",
                root.display(),
                walk.entry_limit,
            ),
        )
        .into());
    }
    Ok(())
}

fn consume_compile_input_file(
    walk: &mut CompileInputWalkState,
    bytes: usize,
    root: &Path,
    description: &str,
) -> Result<(), CompileError> {
    walk.files = walk.files.checked_add(1).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("{description} file count overflow at {}", root.display()),
        )
    })?;
    walk.bytes = walk.bytes.checked_add(bytes).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("{description} byte count overflow at {}", root.display()),
        )
    })?;
    if walk.files > walk.file_limit || walk.bytes > walk.byte_limit {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "{description} tree at {} exceeds its {}-file or {}-byte snapshot budget",
                root.display(),
                walk.file_limit,
                walk.byte_limit,
            ),
        )
        .into());
    }
    Ok(())
}

fn compile_input_symlink_error(path: &Path, description: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!(
            "{description} tree contains unsupported symbolic link at {}",
            path.display(),
        ),
    )
}

fn should_skip_compile_input_dir(path: &Path) -> io::Result<bool> {
    let Some(name) = path.file_name() else {
        return Ok(false);
    };
    let key = super::host_input::portable_host_name_key(name, path)?;
    Ok(matches!(
        key.as_str(),
        ".git" | ".volang" | ".vo-cache" | "node_modules" | "target"
    ))
}

fn should_skip_compile_input_dir_with_options(
    path: &Path,
    tree_kind: CompileInputTreeKind,
    excluded_directories: &BTreeSet<PathBuf>,
) -> io::Result<bool> {
    if portable_path_set_contains(excluded_directories, path)? {
        return Ok(true);
    }
    let Some(name) = path.file_name() else {
        return Ok(false);
    };
    let key = super::host_input::portable_host_name_key(name, path)?;
    Ok(key == ".git"
        || (tree_kind == CompileInputTreeKind::ModuleTree
            && matches!(
                key.as_str(),
                ".volang" | ".vo-cache" | "node_modules" | "target"
            )))
}

fn portable_path_set_contains(paths: &BTreeSet<PathBuf>, candidate: &Path) -> io::Result<bool> {
    for path in paths {
        if super::host_input::portable_host_path_eq(path, candidate)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn is_module_compile_input_file(_root: &Path, path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext == "vo")
        || matches!(
            path.file_name().and_then(|name| name.to_str()),
            Some("vo.mod") | Some("vo.lock") | Some("vo.work")
        )
}

#[cfg(test)]
pub(super) fn try_load_cache(
    slot: &CompileCacheSlot,
    source_root: &Path,
    fingerprint: &str,
) -> Option<CompileOutput> {
    try_load_cache_with_options(
        slot,
        source_root,
        fingerprint,
        &ProjectContextOptions::from_environment(),
    )
}

pub(super) fn try_load_cache_with_options(
    slot: &CompileCacheSlot,
    source_root: &Path,
    fingerprint: &str,
    _workspace_options: &ProjectContextOptions,
) -> Option<CompileOutput> {
    let entry = compile_cache_entry(slot, fingerprint);
    if !entry.dir.is_dir() {
        return None;
    }

    match load_compile_cache_entry(&entry, source_root, fingerprint) {
        Ok(output) => Some(output),
        Err(CacheEntryLoadError::Corrupt) => {
            // A published entry is immutable. Any malformed payload therefore
            // represents a crashed/foreign write or disk corruption. Removing
            // it lets the fallback compile publish one coherent replacement
            // generation. Native provenance is checked against the frozen
            // compile closure by the caller before any cached spec is used.
            let _ = fs::remove_dir_all(&entry.dir);
            None
        }
    }
}

pub(super) fn discard_compile_cache_entry(slot: &CompileCacheSlot, fingerprint: &str) {
    let entry = compile_cache_entry(slot, fingerprint);
    match fs::symlink_metadata(&entry.dir) {
        Ok(metadata) if metadata.file_type().is_dir() => {
            let _ = fs::remove_dir_all(&entry.dir);
        }
        Ok(_) => {
            let _ = fs::remove_file(&entry.dir);
        }
        Err(_) => {}
    }
}

enum CacheEntryLoadError {
    Corrupt,
}

fn load_compile_cache_entry(
    entry: &CompileCacheEntry,
    source_root: &Path,
    fingerprint: &str,
) -> Result<CompileOutput, CacheEntryLoadError> {
    let manifest_bytes =
        read_bounded_cache_file(&entry.manifest_file, COMPILE_CACHE_MANIFEST_MAX_BYTES)
            .map_err(|_| CacheEntryLoadError::Corrupt)?;
    let manifest: CompileCacheEntryManifest =
        serde_json::from_slice(&manifest_bytes).map_err(|_| CacheEntryLoadError::Corrupt)?;
    if manifest.format_version != COMPILE_CACHE_ENTRY_FORMAT_VERSION
        || manifest.fingerprint != fingerprint
    {
        return Err(CacheEntryLoadError::Corrupt);
    }

    let module_bytes =
        read_bounded_cache_file(&entry.module_file, vo_common_core::serialize::MAX_VOB_BYTES)
            .map_err(|_| CacheEntryLoadError::Corrupt)?;
    let extensions_bytes =
        read_bounded_cache_file(&entry.extensions_file, COMPILE_CACHE_EXTENSIONS_MAX_BYTES)
            .map_err(|_| CacheEntryLoadError::Corrupt)?;
    let locked_modules_bytes = read_bounded_cache_file(
        &entry.locked_modules_file,
        COMPILE_CACHE_LOCKED_MODULES_MAX_BYTES,
    )
    .map_err(|_| CacheEntryLoadError::Corrupt)?;
    if payload_digest(&module_bytes) != manifest.module_digest
        || payload_digest(&extensions_bytes) != manifest.extensions_digest
        || payload_digest(&locked_modules_bytes) != manifest.locked_modules_digest
    {
        return Err(CacheEntryLoadError::Corrupt);
    }

    let module = Module::deserialize(&module_bytes).map_err(|_| CacheEntryLoadError::Corrupt)?;
    vo_common_core::verifier::verify_module(&module).map_err(|_| CacheEntryLoadError::Corrupt)?;
    let extensions =
        deserialize_extensions(&extensions_bytes).ok_or(CacheEntryLoadError::Corrupt)?;
    let locked_modules =
        serde_json::from_slice(&locked_modules_bytes).map_err(|_| CacheEntryLoadError::Corrupt)?;

    Ok(CompileOutput {
        module,
        source_root: source_root.to_path_buf(),
        extensions,
        locked_modules,
    })
}

fn read_bounded_cache_file(path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
    super::host_input::read_stable_regular_file(path, max_bytes).map_err(|error| {
        io::Error::new(
            error.kind(),
            format!(
                "failed to read cache payload at {}: {error}",
                path.display()
            ),
        )
    })
}

pub(super) fn save_compile_cache(
    slot: &CompileCacheSlot,
    fingerprint: &str,
    output: &CompileOutput,
) {
    if vo_common_core::verifier::verify_module(&output.module).is_err() {
        return;
    }
    let Ok(module_bytes) = output.module.serialize() else {
        return;
    };
    let Ok(extensions_bytes) = serialize_extensions(&output.extensions) else {
        return;
    };
    let Ok(locked_modules_bytes) = serde_json::to_vec(&output.locked_modules) else {
        return;
    };
    if module_bytes.len() > vo_common_core::serialize::MAX_VOB_BYTES
        || extensions_bytes.len() > COMPILE_CACHE_EXTENSIONS_MAX_BYTES
        || locked_modules_bytes.len() > COMPILE_CACHE_LOCKED_MODULES_MAX_BYTES
    {
        return;
    }
    let manifest = CompileCacheEntryManifest {
        format_version: COMPILE_CACHE_ENTRY_FORMAT_VERSION,
        fingerprint: fingerprint.to_string(),
        module_digest: payload_digest(&module_bytes),
        extensions_digest: payload_digest(&extensions_bytes),
        locked_modules_digest: payload_digest(&locked_modules_bytes),
    };
    let Ok(manifest_bytes) = serde_json::to_vec(&manifest) else {
        return;
    };
    if manifest_bytes.len() > COMPILE_CACHE_MANIFEST_MAX_BYTES {
        return;
    }

    let entry = compile_cache_entry(slot, fingerprint);
    let Some(entries_root) = entry.dir.parent() else {
        return;
    };
    if fs::create_dir_all(entries_root).is_err() {
        return;
    }
    if entry.dir.is_dir() {
        maintain_cache_slot(entries_root, &entry.dir);
        record_test_fingerprint(slot, fingerprint);
        return;
    }

    let Ok(pending) = PendingCacheDir::create(entries_root) else {
        return;
    };
    let pending_entry = cache_entry_at(pending.path.clone());
    let complete = write_synced(&pending_entry.module_file, &module_bytes)
        .and_then(|()| write_synced(&pending_entry.extensions_file, &extensions_bytes))
        .and_then(|()| write_synced(&pending_entry.locked_modules_file, &locked_modules_bytes))
        .and_then(|()| write_synced(&pending_entry.manifest_file, &manifest_bytes))
        .and_then(|()| sync_directory(&pending_entry.dir));
    if complete.is_err() {
        return;
    }

    let published = match fs::rename(&pending_entry.dir, &entry.dir) {
        Ok(()) => {
            let _ = sync_directory(entries_root);
            true
        }
        // Another compiler may have atomically published this exact content
        // identity first. Its immutable entry is equally valid.
        Err(_) => entry.dir.is_dir(),
    };
    if published {
        maintain_cache_slot(entries_root, &entry.dir);
        record_test_fingerprint(slot, fingerprint);
    }
}

fn maintain_cache_slot(entries_root: &Path, current_entry: &Path) {
    let Ok(dir_entries) = fs::read_dir(entries_root) else {
        return;
    };
    let now = std::time::SystemTime::now();
    let mut published = Vec::new();
    for entry in dir_entries.flatten() {
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.starts_with(".tmp-") {
            let stale = entry
                .metadata()
                .and_then(|metadata| metadata.modified())
                .ok()
                .and_then(|modified| now.duration_since(modified).ok())
                .is_some_and(|age| age >= COMPILE_CACHE_STALE_TEMP_AGE);
            if stale {
                let _ = fs::remove_dir_all(path);
            }
            continue;
        }
        if !path.is_dir() || path == current_entry {
            continue;
        }
        let modified = entry
            .metadata()
            .and_then(|metadata| metadata.modified())
            .unwrap_or(std::time::SystemTime::UNIX_EPOCH);
        published.push((modified, name.into_owned(), path));
    }

    published.sort_by(|left, right| right.0.cmp(&left.0).then_with(|| right.1.cmp(&left.1)));
    for (_, _, path) in published
        .into_iter()
        .skip(COMPILE_CACHE_MAX_ENTRIES_PER_SLOT.saturating_sub(1))
    {
        // A concurrent reader may lose this generation between lookup and
        // open; it then follows the ordinary cache-miss fallback. Temp dirs
        // are never considered here, so no in-progress publication is touched.
        let _ = fs::remove_dir_all(path);
    }
}

fn serialize_extensions(extensions: &[NativeExtensionSpec]) -> Result<Vec<u8>, serde_json::Error> {
    let cached = dedupe_extension_specs(extensions.to_vec())
        .iter()
        .map(CachedNativeExtensionSpec::from)
        .collect::<Vec<_>>();
    serde_json::to_vec(&cached)
}

fn deserialize_extensions(bytes: &[u8]) -> Option<Vec<NativeExtensionSpec>> {
    let extensions = serde_json::from_slice::<Vec<CachedNativeExtensionSpec>>(bytes).ok()?;
    let extensions = extensions
        .into_iter()
        .map(NativeExtensionSpec::try_from)
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    Some(dedupe_extension_specs(extensions))
}

fn dedupe_extension_specs(extensions: Vec<NativeExtensionSpec>) -> Vec<NativeExtensionSpec> {
    let mut seen = BTreeSet::new();
    let mut deduped = Vec::with_capacity(extensions.len());
    for spec in extensions {
        let key = (
            spec.name.clone(),
            spec.module_owner.clone(),
            spec.native_path.clone(),
            spec.manifest_path.clone(),
        );
        if seen.insert(key) {
            deduped.push(spec);
        }
    }
    deduped
}

fn payload_digest(bytes: &[u8]) -> String {
    let mut hasher = StableHasher::new(COMPILE_CACHE_PAYLOAD_NAMESPACE);
    hasher.update_bytes("payload", bytes);
    hasher.finish()
}

struct PendingCacheDir {
    path: PathBuf,
}

impl PendingCacheDir {
    fn create(parent: &Path) -> io::Result<Self> {
        loop {
            let nonce = COMPILE_CACHE_TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
            let path = parent.join(format!(".tmp-{}-{nonce:016x}", std::process::id()));
            match fs::create_dir(&path) {
                Ok(()) => return Ok(Self { path }),
                Err(error) if error.kind() == io::ErrorKind::AlreadyExists => continue,
                Err(error) => return Err(error),
            }
        }
    }
}

impl Drop for PendingCacheDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

fn write_synced(path: &Path, bytes: &[u8]) -> io::Result<()> {
    let mut file = OpenOptions::new().write(true).create_new(true).open(path)?;
    file.write_all(bytes)?;
    file.sync_all()
}

#[cfg(unix)]
fn sync_directory(path: &Path) -> io::Result<()> {
    File::open(path)?.sync_all()
}

#[cfg(not(unix))]
fn sync_directory(_path: &Path) -> io::Result<()> {
    Ok(())
}

#[cfg(test)]
fn record_test_fingerprint(slot: &CompileCacheSlot, fingerprint: &str) {
    // Test-only observation aid. Cache reads never consult this marker.
    let _ = fs::write(&slot.fingerprint_file, format!("{fingerprint}\n"));
}

#[cfg(not(test))]
fn record_test_fingerprint(_slot: &CompileCacheSlot, _fingerprint: &str) {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Barrier};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn native_spec(native_path: PathBuf) -> NativeExtensionSpec {
        NativeExtensionSpec::new(
            "demo".to_string(),
            "github.com/acme/demo",
            native_path,
            PathBuf::from("vo.mod"),
        )
    }

    fn temp_cache_slot(name: &str) -> CompileCacheSlot {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time")
            .as_nanos();
        let temp_root = std::env::temp_dir()
            .canonicalize()
            .unwrap_or_else(|_| std::env::temp_dir());
        let dir = temp_root.join(format!("vo-engine-cache-test-{name}-{nonce}"));
        CompileCacheSlot {
            fingerprint_file: dir.join("fingerprint"),
            dir,
        }
    }

    #[test]
    fn compile_input_io_failures_keep_structured_stage_and_kind() {
        let error = classify_compile_input_error(
            io::Error::new(io::ErrorKind::InvalidData, "oversized source").into(),
        );
        let error = error
            .module_system()
            .expect("compile input validation failure must stay structured");
        assert_eq!(error.stage, super::super::ModuleSystemStage::CompileInputs);
        assert_eq!(
            error.kind,
            super::super::ModuleSystemErrorKind::ValidationFailed
        );

        let missing = classify_compile_input_error(
            io::Error::new(io::ErrorKind::NotFound, "missing source").into(),
        );
        assert_eq!(
            missing.module_system().unwrap().kind,
            super::super::ModuleSystemErrorKind::Missing,
        );
    }

    #[test]
    fn compile_input_directory_exclusions_use_portable_aliases() {
        for path in ["module/.GIT", "module/Target", "module/NODE_MODULES"] {
            assert!(should_skip_compile_input_dir(Path::new(path))
                .expect("portable directory exclusion"));
        }

        let excluded = BTreeSet::from([PathBuf::from("module/Native")]);
        assert!(should_skip_compile_input_dir_with_options(
            Path::new("MODULE/native"),
            CompileInputTreeKind::ModuleTree,
            &excluded,
        )
        .expect("portable excluded-root comparison"));
    }

    #[cfg(unix)]
    #[test]
    fn compile_input_directory_exclusions_reject_non_utf8_names() {
        use std::os::unix::ffi::OsStrExt;

        let path = Path::new("module").join(OsStr::from_bytes(b"target\xff"));
        let error = should_skip_compile_input_dir(&path)
            .expect_err("non-UTF-8 exclusion candidates must fail closed");
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
    }

    fn write_raw_entry(
        slot: &CompileCacheSlot,
        fingerprint: &str,
        module_bytes: &[u8],
        extensions_bytes: &[u8],
        locked_modules_bytes: &[u8],
    ) {
        let entry = compile_cache_entry(slot, fingerprint);
        fs::create_dir_all(&entry.dir).expect("create raw cache entry");
        let manifest = CompileCacheEntryManifest {
            format_version: COMPILE_CACHE_ENTRY_FORMAT_VERSION,
            fingerprint: fingerprint.to_string(),
            module_digest: payload_digest(module_bytes),
            extensions_digest: payload_digest(extensions_bytes),
            locked_modules_digest: payload_digest(locked_modules_bytes),
        };
        fs::write(&entry.module_file, module_bytes).expect("write raw module");
        fs::write(&entry.extensions_file, extensions_bytes).expect("write raw extensions");
        fs::write(&entry.locked_modules_file, locked_modules_bytes)
            .expect("write raw locked modules");
        fs::write(
            &entry.manifest_file,
            serde_json::to_vec(&manifest).expect("serialize raw manifest"),
        )
        .expect("write raw manifest");
    }

    #[test]
    fn native_input_discovery_rejects_partially_valid_mod_metadata() {
        let slot = temp_cache_slot("invalid-mod-metadata");
        let root = slot.dir.join("project");
        fs::create_dir_all(&root).unwrap();
        fs::write(
            root.join("vo.mod"),
            "[extension]\nname = \"demo\"\n[extension.native]\ntargets = [\"aarch64-apple-darwin\"]\n",
        )
        .unwrap();

        let root_directory = super::super::host_input::read_stable_directory(
            &root,
            COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
        )
        .expect("open discovery root");
        let error = match discover_native_module_inputs(&root, root_directory, &BTreeSet::new()) {
            Ok(_) => panic!("partially valid vo.mod metadata was accepted"),
            Err(error) => error,
        };

        assert!(
            error.to_string().contains("missing or non-string 'module'"),
            "{error}"
        );
        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn locked_module_input_collection_includes_frozen_readiness_metadata_and_target_artifacts() {
        let slot = temp_cache_slot("nested-locked-artifact");
        let root = slot.dir.join("module");
        let artifact = vo_module::schema::manifest::ManifestArtifact {
            id: vo_module::identity::ArtifactId {
                kind: "extension-native".to_string(),
                target: current_target_triple().to_string(),
                name: "libdemo.bin".to_string(),
            },
            size: 4,
            digest: vo_module::digest::Digest::from_sha256(b"demo"),
        };
        let artifact_id = artifact.id.clone();
        let release = vo_module::schema::manifest::ReleaseManifest {
            schema_version: 2,
            module: vo_module::identity::ModulePath::parse("github.com/acme/demo").unwrap(),
            version: vo_module::version::ExactVersion::parse("1.0.0").unwrap(),
            vo: vo_module::version::ToolchainConstraint::parse("^0.1.0").unwrap(),
            commit: "1111111111111111111111111111111111111111".to_string(),
            dependencies: Vec::new(),
            source: vo_module::schema::manifest::ManifestSource {
                name: "source.tar.gz".to_string(),
                size: 3,
                digest: vo_module::digest::Digest::from_sha256(b"src"),
            },
            package: vo_module::schema::manifest::ManifestPackage {
                size: 3,
                digest: vo_module::digest::Digest::from_sha256(b"pkg"),
            },
            artifacts: vec![artifact],
        };
        let release_raw = release.render().unwrap();
        let locked = LockedModule {
            path: release.module.clone(),
            version: release.version.clone(),
            vo: release.vo.clone(),
            release: vo_module::digest::Digest::from_sha256(release_raw.as_bytes()),
            dependencies: Vec::new(),
        };
        let relative_artifact = vo_module::artifact::artifact_relative_path(&artifact_id).unwrap();
        let artifact_path = root.join(&relative_artifact);
        fs::create_dir_all(artifact_path.parent().unwrap()).unwrap();
        fs::write(&artifact_path, b"demo").unwrap();
        fs::write(
            root.join(vo_module::cache::layout::VERSION_MARKER),
            b"1.0.0\n",
        )
        .unwrap();
        fs::write(
            root.join(vo_module::cache::layout::SOURCE_DIGEST_MARKER),
            format!("{}\n", release.source.digest),
        )
        .unwrap();
        fs::write(root.join("vo.mod"), b"").unwrap();
        fs::write(root.join("vo.package.json"), b"{}").unwrap();
        fs::write(root.join("vo.release.json"), &release_raw).unwrap();
        fs::create_dir_all(root.join("docs")).unwrap();
        fs::write(
            root.join("docs")
                .join(vo_module::cache::layout::VERSION_MARKER),
            b"nested marker",
        )
        .unwrap();
        fs::write(root.join("README.md"), b"not a compile input").unwrap();

        let artifact_paths =
            locked_module_artifact_input_paths(&locked, release_raw.as_bytes()).unwrap();
        let mut files = Vec::new();
        let root_directory = super::super::host_input::read_stable_directory(
            &root,
            COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
        )
        .expect("open locked module root");
        collect_locked_module_input_files(
            &root,
            &root,
            root_directory,
            &artifact_paths,
            &mut files,
            super::super::snapshot::MAX_COMPILE_SNAPSHOT_FILES,
            super::super::snapshot::MAX_COMPILE_SNAPSHOT_BYTES,
        )
        .unwrap();

        assert_eq!(
            files
                .into_iter()
                .map(|file| file.relative)
                .collect::<BTreeSet<_>>(),
            BTreeSet::from([
                PathBuf::from(vo_module::cache::layout::SOURCE_DIGEST_MARKER),
                PathBuf::from(vo_module::cache::layout::VERSION_MARKER),
                relative_artifact,
                PathBuf::from("vo.mod"),
                PathBuf::from("vo.package.json"),
                PathBuf::from("vo.release.json"),
            ])
        );
        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn cached_extensions_dedupe_duplicate_specs_on_load() {
        let slot = temp_cache_slot("dedupe-extensions");
        let cached = vec![
            CachedNativeExtensionSpec {
                name: "demo".to_string(),
                module_owner: "github.com/acme/demo".to_string(),
                native_path: PathBuf::from("rust/target/debug/libdemo.dylib"),
                manifest_path: PathBuf::from("vo.mod"),
            },
            CachedNativeExtensionSpec {
                name: "demo".to_string(),
                module_owner: "github.com/acme/demo".to_string(),
                native_path: PathBuf::from("rust/target/debug/libdemo.dylib"),
                manifest_path: PathBuf::from("vo.mod"),
            },
        ];
        let loaded = deserialize_extensions(&serde_json::to_vec(&cached).unwrap())
            .expect("load extensions cache");
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].name, "demo");

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn cached_extensions_reject_noncanonical_module_owners() {
        let cached = vec![CachedNativeExtensionSpec {
            name: "demo".to_string(),
            module_owner: "github.com/acme//demo".to_string(),
            native_path: PathBuf::from("rust/target/debug/libdemo.dylib"),
            manifest_path: PathBuf::from("vo.mod"),
        }];

        assert!(deserialize_extensions(&serde_json::to_vec(&cached).unwrap()).is_none());
    }

    #[test]
    fn invalid_cached_module_is_ignored() {
        let slot = temp_cache_slot("invalid-module");
        let invalid_module = Module::new("invalid-cache".to_string())
            .serialize()
            .expect("serialize invalid cache fixture");
        write_raw_entry(&slot, "fingerprint", &invalid_module, b"[]", b"[]");

        assert!(
            try_load_cache(&slot, Path::new("."), "fingerprint").is_none(),
            "invalid cached bytecode must not be returned"
        );
        assert!(!compile_cache_entry(&slot, "fingerprint").dir.exists());

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[cfg(unix)]
    #[test]
    fn cache_payload_symlinks_are_rejected_even_when_bytes_match() {
        use std::os::unix::fs::symlink;

        let slot = temp_cache_slot("payload-symlink");
        let source_root = slot.dir.join("sources");
        let output = crate::compile_source_at("package main\nfunc main() {}\n", &source_root)
            .expect("compile cache fixture");
        let fingerprint = "payload-symlink";
        save_compile_cache(&slot, fingerprint, &output);
        let entry = compile_cache_entry(&slot, fingerprint);
        let external = slot.dir.join("external-module.voc");
        fs::copy(&entry.module_file, &external).expect("copy cache payload bytes");
        fs::remove_file(&entry.module_file).expect("replace cache payload");
        symlink(&external, &entry.module_file).expect("link cache payload");

        assert!(try_load_cache(&slot, &source_root, fingerprint).is_none());
        assert!(!entry.dir.exists(), "linked immutable entry is removed");
        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn oversized_cache_payloads_are_rejected_without_allocation_or_panic() {
        let slot = temp_cache_slot("oversized-payloads");
        let source_root = slot.dir.join("sources");
        let output = crate::compile_source_at("package main\nfunc main() {}\n", &source_root)
            .expect("compile cache fixture");

        for (index, payload, max_bytes) in [
            ("manifest", "manifest", COMPILE_CACHE_MANIFEST_MAX_BYTES),
            ("module", "module", vo_common_core::serialize::MAX_VOB_BYTES),
            (
                "extensions",
                "extensions",
                COMPILE_CACHE_EXTENSIONS_MAX_BYTES,
            ),
            (
                "locked-modules",
                "locked-modules",
                COMPILE_CACHE_LOCKED_MODULES_MAX_BYTES,
            ),
        ] {
            let fingerprint = format!("oversized-{index}");
            save_compile_cache(&slot, &fingerprint, &output);
            let entry = compile_cache_entry(&slot, &fingerprint);
            let path = match payload {
                "manifest" => &entry.manifest_file,
                "module" => &entry.module_file,
                "extensions" => &entry.extensions_file,
                "locked-modules" => &entry.locked_modules_file,
                _ => unreachable!(),
            };
            OpenOptions::new()
                .write(true)
                .open(path)
                .expect("open cache payload")
                .set_len(u64::try_from(max_bytes).unwrap() + 1)
                .expect("make sparse oversized payload");

            assert!(try_load_cache(&slot, &source_root, &fingerprint).is_none());
            assert!(!entry.dir.exists(), "oversized entry should be removed");
        }

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn concurrent_distinct_fingerprints_publish_isolated_complete_entries() {
        let slot = temp_cache_slot("concurrent-publish");
        let source_root = slot.dir.join("sources");
        let output_a = crate::compile_source_at(
            "package main\nfunc main() {}\nfunc onlyA() {}\n",
            &source_root.join("a"),
        )
        .expect("compile fixture A");
        let output_b = crate::compile_source_at(
            "package main\nfunc main() {}\nfunc onlyB() {}\n",
            &source_root.join("b"),
        )
        .expect("compile fixture B");
        let expected_a = output_a.module.serialize().expect("serialize fixture A");
        let expected_b = output_b.module.serialize().expect("serialize fixture B");
        let barrier = Arc::new(Barrier::new(3));

        std::thread::scope(|scope| {
            let slot_a = slot.clone();
            let barrier_a = Arc::clone(&barrier);
            scope.spawn(move || {
                barrier_a.wait();
                save_compile_cache(&slot_a, "fingerprint-a", &output_a);
            });
            let slot_b = slot.clone();
            let barrier_b = Arc::clone(&barrier);
            scope.spawn(move || {
                barrier_b.wait();
                save_compile_cache(&slot_b, "fingerprint-b", &output_b);
            });
            barrier.wait();
        });

        let loaded_a = try_load_cache(&slot, &source_root, "fingerprint-a")
            .expect("load concurrently published fixture A");
        let loaded_b = try_load_cache(&slot, &source_root, "fingerprint-b")
            .expect("load concurrently published fixture B");
        assert_eq!(
            loaded_a.module.serialize().expect("serialize loaded A"),
            expected_a
        );
        assert_eq!(
            loaded_b.module.serialize().expect("serialize loaded B"),
            expected_b
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn mixed_payload_is_rejected_and_the_entry_can_be_healed() {
        let slot = temp_cache_slot("mixed-payload");
        let source_root = slot.dir.join("sources");
        let output_a = crate::compile_source_at(
            "package main\nfunc main() {}\nfunc onlyA() {}\n",
            &source_root.join("a"),
        )
        .expect("compile fixture A");
        let output_b = crate::compile_source_at(
            "package main\nfunc main() {}\nfunc onlyB() {}\n",
            &source_root.join("b"),
        )
        .expect("compile fixture B");
        let expected_a = output_a.module.serialize().expect("serialize fixture A");

        save_compile_cache(&slot, "fingerprint-a", &output_a);
        let entry = compile_cache_entry(&slot, "fingerprint-a");
        fs::write(
            &entry.module_file,
            output_b.module.serialize().expect("serialize fixture B"),
        )
        .expect("replace module with a valid mismatched payload");

        assert!(try_load_cache(&slot, &source_root, "fingerprint-a").is_none());
        assert!(!entry.dir.exists(), "corrupt immutable entry is removed");

        save_compile_cache(&slot, "fingerprint-a", &output_a);
        let healed =
            try_load_cache(&slot, &source_root, "fingerprint-a").expect("load healed cache entry");
        assert_eq!(
            healed.module.serialize().expect("serialize healed module"),
            expected_a
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn native_artifact_provenance_is_deferred_to_the_frozen_compile_caller() {
        let slot = temp_cache_slot("unavailable-artifact");
        let source_root = slot.dir.join("sources");
        let mut output = crate::compile_source_at("package main\nfunc main() {}\n", &source_root)
            .expect("compile cache fixture");
        output.extensions = vec![native_spec(slot.dir.join("missing/libdemo.dylib"))];

        let fingerprint = "same-source-fingerprint";
        save_compile_cache(&slot, fingerprint, &output);
        let entry = compile_cache_entry(&slot, fingerprint);
        assert!(entry.dir.is_dir());
        assert!(try_load_cache(&slot, &source_root, fingerprint).is_some());
        assert!(entry.dir.is_dir());

        discard_compile_cache_entry(&slot, fingerprint);
        assert!(
            !entry.dir.exists(),
            "the caller can discard an entry after frozen provenance validation fails"
        );

        output.extensions.clear();
        save_compile_cache(&slot, fingerprint, &output);
        assert!(try_load_cache(&slot, &source_root, fingerprint).is_some());

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn cache_slot_retention_is_bounded_and_skips_active_temp_directories() {
        let slot = temp_cache_slot("bounded-retention");
        let source_root = slot.dir.join("sources");
        let output = crate::compile_source_at("package main\nfunc main() {}\n", &source_root)
            .expect("compile retention fixture");
        fs::create_dir_all(&slot.dir).expect("create cache slot");
        let entries_root = slot.dir.join("entries");
        fs::create_dir_all(&entries_root).expect("create entries root");
        let active_temp = entries_root.join(".tmp-active");
        fs::create_dir(&active_temp).expect("create active temp dir");

        for index in 0..(COMPILE_CACHE_MAX_ENTRIES_PER_SLOT + 2) {
            save_compile_cache(&slot, &format!("fingerprint-{index}"), &output);
        }

        let published = fs::read_dir(&entries_root)
            .expect("read entries root")
            .filter_map(Result::ok)
            .filter(|entry| {
                entry.path().is_dir() && !entry.file_name().to_string_lossy().starts_with(".tmp-")
            })
            .count();
        assert_eq!(published, COMPILE_CACHE_MAX_ENTRIES_PER_SLOT);
        assert!(
            active_temp.is_dir(),
            "active temp directory must be preserved"
        );
        let newest = format!("fingerprint-{}", COMPILE_CACHE_MAX_ENTRIES_PER_SLOT + 1);
        assert!(compile_cache_entry(&slot, &newest).dir.is_dir());
        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn stdlib_source_identity_participates_in_compile_cache_fingerprint() {
        let slot = temp_cache_slot("stdlib-fingerprint");
        let project_root = slot.dir.join("project");
        let mod_cache = slot.dir.join("mod-cache");
        fs::create_dir_all(&project_root).expect("create project root");
        fs::create_dir_all(&mod_cache).expect("create module cache");
        fs::write(
            project_root.join("main.vo"),
            "package main\nfunc main() {}\n",
        )
        .expect("write source");

        let entry = project_root.join("main.vo");
        let project_deps = ProjectDeps::default();
        let workspace_sources = HashMap::new();
        let workspace_options =
            ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled);

        let fingerprint = |stdlib_identity: &str| {
            capture_compile_inputs(CompileInputCapture {
                source_root: &project_root,
                project_root: &project_root,
                mod_cache: &mod_cache,
                single_file: Some(&entry),
                single_file_source_generation: None,
                graph: &ProjectGraphContext::empty(),
                project_deps: &project_deps,
                workspace_sources: &workspace_sources,
                workspace_options: &workspace_options,
                workspace_generation: "",
                stdlib_source_fingerprint: stdlib_identity,
            })
            .expect("compute compile fingerprint")
            .fingerprint()
            .to_string()
        };

        let original = fingerprint("sha256:stdlib-a");
        assert_eq!(original, fingerprint("sha256:stdlib-a"));
        let changed = fingerprint("sha256:stdlib-b");
        assert_ne!(original, changed);

        let output = crate::compile_source_at("package main\nfunc main() {}\n", &project_root)
            .expect("compile cache fixture");
        save_compile_cache(&slot, &original, &output);
        assert!(try_load_cache(&slot, &project_root, &original).is_some());
        assert!(
            try_load_cache(&slot, &project_root, &changed).is_none(),
            "a same-process stdlib identity change must miss the old entry"
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn cargo_native_inputs_are_deferred_and_generated_cache_dirs_are_excluded() {
        let slot = temp_cache_slot("custom-native-manifest-tree");
        let project_root = slot.dir.join("project");
        let mod_cache = slot.dir.join("mod-cache");
        let native_dir = project_root.join("native/ext");
        fs::create_dir_all(native_dir.join("src")).expect("create native source tree");
        fs::create_dir_all(&mod_cache).expect("create module cache");
        fs::write(
            project_root.join("vo.mod"),
            format!(
                concat!(
                    "module = \"github.com/acme/demo\"\n",
                    "vo = \"^1.0.0\"\n\n",
                    "[extension]\n",
                    "name = \"demo\"\n\n",
                    "[extension.native]\n",
                    "targets = [\"{}\"]\n\n",
                    "[build.native]\n",
                    "kind = \"cargo\"\n",
                    "manifest = \"native/ext/Cargo.toml\"\n",
                ),
                current_target_triple(),
            ),
        )
        .expect("write module manifest");
        fs::write(project_root.join("main.vo"), "package demo\n").expect("write source");
        fs::write(
            native_dir.join("Cargo.toml"),
            concat!(
                "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n",
                "[lib]\ncrate-type = [\"cdylib\"]\n",
            ),
        )
        .expect("write Cargo manifest");
        let native_source = native_dir.join("src/lib.rs");
        fs::write(&native_source, "pub fn value() -> u8 { 1 }\n").expect("write native source");
        let cargo_cache_input = native_dir.join("node_modules/generator/input.txt");
        fs::create_dir_all(cargo_cache_input.parent().expect("Cargo input parent"))
            .expect("create Cargo input tree");
        fs::write(&cargo_cache_input, "one\n").expect("write Cargo input");
        fs::create_dir_all(project_root.join(".cargo")).expect("create Cargo config dir");
        fs::write(
            project_root.join(".cargo/config.toml"),
            "[build]\ntarget-dir = \"native/cargo-out\"\n",
        )
        .expect("write Cargo target config");
        let cargo_output = project_root.join("native/cargo-out/debug/generated.vo");
        fs::create_dir_all(cargo_output.parent().expect("Cargo output parent"))
            .expect("create custom Cargo output tree");
        fs::write(
            project_root.join("native/cargo-out/CACHEDIR.TAG"),
            b"Signature: 8a477f597d28d172789f06886806bc55\n# Cargo target cache\n",
        )
        .expect("mark root custom Cargo target cache");
        fs::write(&cargo_output, "generated one\n").expect("write custom Cargo output");
        let nested_module = project_root.join("nested");
        let nested_cargo = nested_module.join("cargo");
        fs::create_dir_all(nested_cargo.join("src")).expect("create nested Cargo tree");
        fs::write(
            nested_module.join("vo.mod"),
            format!(
                concat!(
                    "module = \"github.com/acme/nested\"\n",
                    "vo = \"^1.0.0\"\n\n",
                    "[extension]\n",
                    "name = \"nested\"\n\n",
                    "[extension.native]\n",
                    "targets = [\"{}\"]\n\n",
                    "[build.native]\n",
                    "kind = \"cargo\"\n",
                    "manifest = \"cargo/Cargo.toml\"\n",
                ),
                current_target_triple(),
            ),
        )
        .expect("write nested module manifest");
        fs::write(
            nested_cargo.join("Cargo.toml"),
            concat!(
                "[package]\nname = \"nested\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n",
                "[lib]\ncrate-type = [\"cdylib\"]\n",
            ),
        )
        .expect("write nested Cargo manifest");
        fs::write(nested_cargo.join("src/lib.rs"), "pub fn nested() {}\n")
            .expect("write nested Cargo source");
        fs::create_dir_all(nested_module.join(".cargo")).expect("create nested Cargo config dir");
        fs::write(
            nested_module.join(".cargo/config.toml"),
            "[build]\ntarget-dir = \"custom-build\"\n",
        )
        .expect("write nested Cargo target config");
        let nested_output = nested_module.join("custom-build/debug/generated.vo");
        fs::create_dir_all(nested_output.parent().expect("nested output parent"))
            .expect("create nested custom output tree");
        fs::write(
            nested_module.join("custom-build/CACHEDIR.TAG"),
            b"Signature: 8a477f597d28d172789f06886806bc55\n# Cargo target cache\n",
        )
        .expect("mark nested custom Cargo target cache");
        fs::write(&nested_output, "nested generated one\n").expect("write nested custom output");

        let capture = || {
            let mut hasher = StableHasher::new("custom-native-manifest-tree-test");
            let mut snapshot = CompileInputSnapshot::default();
            let native_modules =
                capture_compile_input_tree(&mut hasher, &mut snapshot, "project", &project_root)
                    .expect("capture compile tree");
            (hasher.finish(), native_modules)
        };
        let project_deps = ProjectDeps::default();
        let workspace_sources = HashMap::new();
        let workspace_options =
            ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled);
        let entry = project_root.join("main.vo");
        let capture_full = || {
            capture_compile_inputs(CompileInputCapture {
                source_root: &project_root,
                project_root: &project_root,
                mod_cache: &mod_cache,
                single_file: Some(&entry),
                single_file_source_generation: None,
                graph: &ProjectGraphContext::empty(),
                project_deps: &project_deps,
                workspace_sources: &workspace_sources,
                workspace_options: &workspace_options,
                workspace_generation: "",
                stdlib_source_fingerprint: "sha256:stdlib",
            })
            .expect("capture complete compile inputs")
            .fingerprint()
            .to_string()
        };
        let (initial, native_modules) = capture();
        let full_initial = capture_full();
        assert_eq!(
            native_modules,
            BTreeMap::from([
                (
                    project_root.canonicalize().expect("canonical project root"),
                    native_dir
                        .join("Cargo.toml")
                        .canonicalize()
                        .expect("canonical root Cargo manifest"),
                ),
                (
                    nested_module
                        .canonicalize()
                        .expect("canonical nested module"),
                    nested_cargo
                        .join("Cargo.toml")
                        .canonicalize()
                        .expect("canonical nested Cargo manifest"),
                ),
            ]),
        );

        fs::write(&cargo_output, "generated two\n").expect("change custom Cargo output");
        fs::write(&nested_output, "nested generated two\n")
            .expect("change nested custom Cargo output");
        let (output_changed, _) = capture();
        let full_output_changed = capture_full();
        assert_eq!(initial, output_changed);
        assert_eq!(full_initial, full_output_changed);

        fs::write(&cargo_cache_input, "two\n").expect("change legal Cargo cache-tree input");
        let (cargo_input_changed, _) = capture();
        let full_cargo_input_changed = capture_full();
        assert_eq!(output_changed, cargo_input_changed);
        assert_eq!(full_output_changed, full_cargo_input_changed);

        fs::write(&native_source, "pub fn value() -> u8 { 2 }\n").expect("change native source");
        let (changed, _) = capture();
        let full_changed = capture_full();
        assert_eq!(cargo_input_changed, changed);
        assert_eq!(full_cargo_input_changed, full_changed);

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn prebuilt_native_bytes_are_deferred_until_the_extension_is_reached() {
        let slot = temp_cache_slot("deferred-prebuilt-native");
        let root = slot.dir.join("module");
        let prebuilt = root.join("dist/libdemo.native");
        fs::create_dir_all(prebuilt.parent().unwrap()).expect("create prebuilt directory");
        fs::write(
            root.join("vo.mod"),
            format!(
                concat!(
                    "module = \"github.com/acme/prebuilt\"\n",
                    "vo = \"^1.0.0\"\n\n",
                    "[extension]\nname = \"prebuilt\"\n\n",
                    "[extension.native]\ntargets = [\"{}\"]\n\n",
                    "[build.native]\nkind = \"prebuilt\"\n",
                    "path = \"dist/libdemo.native\"\n",
                ),
                current_target_triple(),
            ),
        )
        .expect("write module manifest");
        fs::write(root.join("main.vo"), "package prebuilt\n").expect("write Vo source");
        fs::write(&prebuilt, b"first prebuilt generation").expect("write prebuilt bytes");

        let capture = || {
            let mut hasher = StableHasher::new("deferred-prebuilt-native-test");
            let mut snapshot = CompileInputSnapshot::default();
            capture_compile_input_tree(&mut hasher, &mut snapshot, "module", &root)
                .expect("capture module without reading prebuilt bytes");
            (hasher.finish(), snapshot)
        };

        let (initial, snapshot) = capture();
        assert!(snapshot.read_file(&prebuilt).is_err());
        fs::write(&prebuilt, b"second prebuilt generation").expect("change prebuilt bytes");
        assert_eq!(initial, capture().0);

        #[cfg(unix)]
        {
            use std::os::unix::fs::symlink;

            fs::remove_file(&prebuilt).expect("remove prebuilt file");
            symlink("../main.vo", &prebuilt).expect("replace prebuilt with invalid symlink");
            assert_eq!(
                initial,
                capture().0,
                "an invalid prebuilt generation must fail only when its extension is reached",
            );
        }

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn reserved_native_root_is_opaque_until_the_extension_is_reached() {
        let slot = temp_cache_slot("custom-cargo-manifest");
        let root = slot.dir.join("module");
        let native = root.join("native");
        let cargo_dir = native.join("tooling/deep");
        fs::create_dir_all(&cargo_dir).expect("create custom Cargo directory");
        fs::write(
            root.join("vo.mod"),
            format!(
                concat!(
                    "module = \"github.com/acme/root-cargo\"\n",
                    "vo = \"^1.0.0\"\n\n",
                    "[extension]\nname = \"root-cargo\"\n\n",
                    "[extension.native]\ntargets = [\"{}\"]\n\n",
                    "[build.native]\nkind = \"cargo\"\nmanifest = \"native/tooling/deep/Cargo.toml\"\n",
                ),
                current_target_triple(),
            ),
        )
        .expect("write module manifest");
        fs::write(root.join("main.vo"), "package rootcargo\n").expect("write Vo source");
        // `native` is reserved by the first manifest-path component. Files
        // below it are invalid language inputs and must stay out of the base
        // snapshot even when they have language-shaped names.
        fs::write(native.join("helper.vo"), "package helper\n").expect("write reserved Vo file");
        fs::write(
            cargo_dir.join("Cargo.toml"),
            "intentionally invalid Cargo input\n",
        )
        .expect("write unused Cargo manifest");
        fs::create_dir_all(cargo_dir.join("src")).expect("create native source directory");
        fs::write(cargo_dir.join("src/lib.rs"), "pub fn native() {}\n")
            .expect("write unused native source");
        let mut deep_vendor = native.join("vendor");
        for _ in 0..=COMPILE_INPUT_MAX_DIRECTORY_DEPTH {
            deep_vendor.push("d");
        }
        fs::create_dir_all(&deep_vendor).expect("create over-depth vendored Rust tree");
        fs::write(deep_vendor.join("lib.rs"), "pub fn vendored() {}\n")
            .expect("write vendored Rust source");
        #[cfg(unix)]
        {
            use std::os::unix::fs::symlink;
            symlink("../main.vo", native.join("unrelated-source-link"))
                .expect("create unrelated native symlink");
        }

        let mut hasher = StableHasher::new("module-root-cargo-test");
        let mut snapshot = CompileInputSnapshot::default();
        let native_modules =
            capture_compile_input_tree(&mut hasher, &mut snapshot, "module", &root)
                .expect("capture module root without invoking Cargo");

        assert_eq!(
            snapshot.read_file(&root.join("main.vo")).unwrap(),
            "package rootcargo\n",
        );
        assert!(snapshot.read_file(&native.join("helper.vo")).is_err());
        assert!(snapshot.read_file(&cargo_dir.join("Cargo.toml")).is_err());
        assert!(snapshot.read_file(&cargo_dir.join("src/lib.rs")).is_err());
        assert_eq!(
            super::super::host_input::directory_enumeration_count(&native),
            0,
            "base capture must not enumerate a reserved native root",
        );
        assert_eq!(
            native_modules,
            BTreeMap::from([(root.clone(), cargo_dir.join("Cargo.toml"))]),
        );

        let initial_fingerprint = hasher.finish();
        fs::write(
            native.join("helper.vo"),
            "package helper\nconst Changed = true\n",
        )
        .expect("change nested Vo source");
        let mut changed_hasher = StableHasher::new("module-root-cargo-test");
        let mut changed_snapshot = CompileInputSnapshot::default();
        capture_compile_input_tree(&mut changed_hasher, &mut changed_snapshot, "module", &root)
            .expect("recapture changed nested Vo source");
        assert_eq!(initial_fingerprint, changed_hasher.finish());
        assert_eq!(
            super::super::host_input::directory_enumeration_count(&native),
            0,
        );
        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn compile_input_capture_keeps_source_and_project_roots_distinct() {
        let slot = temp_cache_slot("capture-root-roles");
        let project_root = slot.dir.join("project");
        let source_root = project_root.join("cmd");
        let mod_cache = slot.dir.join("mod-cache");
        fs::create_dir_all(&source_root).expect("create source root");
        fs::create_dir_all(&mod_cache).expect("create module cache");
        let entry = source_root.join("main.vo");
        fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
        let project_deps = ProjectDeps::default();
        let workspace_sources = HashMap::new();
        let workspace_options =
            ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled);

        let fingerprint = |captured_source_root: &Path| {
            capture_compile_inputs(CompileInputCapture {
                source_root: captured_source_root,
                project_root: &project_root,
                mod_cache: &mod_cache,
                single_file: Some(&entry),
                single_file_source_generation: None,
                graph: &ProjectGraphContext::empty(),
                project_deps: &project_deps,
                workspace_sources: &workspace_sources,
                workspace_options: &workspace_options,
                workspace_generation: "",
                stdlib_source_fingerprint: "sha256:stdlib",
            })
            .expect("capture compile inputs")
            .fingerprint()
            .to_string()
        };

        assert_ne!(fingerprint(&source_root), fingerprint(&project_root));

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn compile_input_capture_rejects_drift_between_full_tree_scans() {
        let slot = temp_cache_slot("capture-drift");
        let project_root = slot.dir.join("project");
        let mod_cache = slot.dir.join("mod-cache");
        fs::create_dir_all(&project_root).expect("create project root");
        fs::create_dir_all(&mod_cache).expect("create module cache");
        let entry = project_root.join("main.vo");
        fs::write(&entry, "package main\nfunc main() {}\n").expect("write source");
        let project_deps = ProjectDeps::default();
        let graph = ProjectGraphContext::empty();
        let workspace_sources = HashMap::new();
        let workspace_options =
            ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled);
        let input = CompileInputCapture {
            source_root: &project_root,
            project_root: &project_root,
            mod_cache: &mod_cache,
            single_file: Some(&entry),
            single_file_source_generation: None,
            graph: &graph,
            project_deps: &project_deps,
            workspace_sources: &workspace_sources,
            workspace_options: &workspace_options,
            workspace_generation: "",
            stdlib_source_fingerprint: "sha256:stdlib",
        };

        let result = capture_compile_inputs_with_between_scans(input, || {
            fs::write(&entry, "package main\nfunc main() { println(1) }\n")
                .expect("change source between captures");
        });
        let error = match result {
            Ok(_) => panic!("mixed full-tree generations were accepted"),
            Err(error) => error,
        };
        assert!(error
            .to_string()
            .contains("between its two bounded captures"));

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn compile_input_capture_rejects_single_file_classification_generation_drift() {
        let slot = temp_cache_slot("single-file-classification-drift");
        let project_root = slot.dir.join("project");
        let mod_cache = slot.dir.join("mod-cache");
        fs::create_dir_all(&project_root).expect("create project root");
        fs::create_dir_all(&mod_cache).expect("create module cache");
        let entry = project_root.join("main.vo");
        fs::write(&entry, "package main\nfunc main() {}\n").expect("write ad-hoc source");
        let project_deps = ProjectDeps::default();
        let graph = ProjectGraphContext::empty();
        let workspace_sources = HashMap::new();
        let workspace_options =
            ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled);
        let (_, source_generation) =
            vo_module::project::load_single_file_context_with_options_and_generation(
                &RealFs::new("."),
                &entry,
                &workspace_options,
            )
            .expect("classify stable single-file source");
        let input = CompileInputCapture {
            source_root: &project_root,
            project_root: &project_root,
            mod_cache: &mod_cache,
            single_file: Some(&entry),
            single_file_source_generation: Some(&source_generation),
            graph: &graph,
            project_deps: &project_deps,
            workspace_sources: &workspace_sources,
            workspace_options: &workspace_options,
            workspace_generation: "",
            stdlib_source_fingerprint: "sha256:stdlib",
        };

        let result = capture_compile_inputs_with_between_scans(input, || {
            fs::write(
                &entry,
                "/*vo:mod\nmodule = \"local/raced\"\nvo = \"^0.1.0\"\n*/\npackage main\nfunc main() {}\n",
            )
            .expect("change classification between captures");
        });
        let error = match result {
            Ok(_) => panic!("mixed single-file classification generation was accepted"),
            Err(error) => error,
        };
        assert_eq!(
            error.module_system().map(|error| error.stage),
            Some(super::super::ModuleSystemStage::CompileInputs),
        );
        assert!(
            error
                .to_string()
                .contains("single-file project authority changed after classification"),
            "{error}",
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn compile_input_capture_rejects_ancestor_project_authority_appearance() {
        let slot = temp_cache_slot("single-file-ancestor-authority-drift");
        let ancestor = slot.dir.join("ancestor");
        let project_root = ancestor.join("source");
        let mod_cache = slot.dir.join("mod-cache");
        fs::create_dir_all(&project_root).expect("create source root");
        fs::create_dir_all(&mod_cache).expect("create module cache");
        let entry = project_root.join("main.vo");
        fs::write(&entry, "package main\nfunc main() {}\n").expect("write ad-hoc source");
        let project_deps = ProjectDeps::default();
        let graph = ProjectGraphContext::empty();
        let workspace_sources = HashMap::new();
        let workspace_options =
            ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled);
        let (_, source_generation) =
            vo_module::project::load_single_file_context_with_options_and_generation(
                &RealFs::new("."),
                &entry,
                &workspace_options,
            )
            .expect("classify ad-hoc source before ancestor metadata appears");
        let input = CompileInputCapture {
            source_root: &project_root,
            project_root: &project_root,
            mod_cache: &mod_cache,
            single_file: Some(&entry),
            single_file_source_generation: Some(&source_generation),
            graph: &graph,
            project_deps: &project_deps,
            workspace_sources: &workspace_sources,
            workspace_options: &workspace_options,
            workspace_generation: "",
            stdlib_source_fingerprint: "sha256:stdlib",
        };

        let result = capture_compile_inputs_with_between_scans(input, || {
            fs::write(
                ancestor.join("vo.mod"),
                "module = \"github.com/acme/appeared\"\nvo = \"^0.1.0\"\n",
            )
            .expect("create ancestor project authority between captures");
        });
        let error = match result {
            Ok(_) => panic!("ancestor project authority appearance was accepted"),
            Err(error) => error,
        };
        assert_eq!(
            error.module_system().map(|error| error.stage),
            Some(super::super::ModuleSystemStage::CompileInputs),
        );
        assert!(
            error
                .to_string()
                .contains("single-file project authority changed after classification"),
            "{error}",
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn module_compile_input_walk_honors_caller_entry_budget() {
        let slot = temp_cache_slot("module-input-budget");
        let root = slot.dir.join("project");
        fs::create_dir_all(&root).expect("create project root");
        fs::write(root.join("a.vo"), "package a\n").expect("write source a");
        fs::write(root.join("b.vo"), "package b\n").expect("write source b");

        let error = collect_module_compile_input_files(
            &root,
            1,
            super::super::snapshot::MAX_COMPILE_SNAPSHOT_FILES,
            super::super::snapshot::MAX_COMPILE_SNAPSHOT_BYTES,
        )
        .expect_err("caller entry budget must bound one module tree");
        assert!(error.to_string().contains("entry limit"), "{error}");

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn module_compile_input_walk_enforces_file_and_byte_budgets_while_reading() {
        let slot = temp_cache_slot("module-input-size-budget");
        let root = slot.dir.join("project");
        fs::create_dir_all(&root).expect("create project root");
        fs::write(root.join("a.vo"), "package a\n").expect("write source a");
        fs::write(root.join("b.vo"), "package b\n").expect("write source b");

        let file_error = collect_module_compile_input_files(
            &root,
            COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
            1,
            super::super::snapshot::MAX_COMPILE_SNAPSHOT_BYTES,
        )
        .expect_err("caller file budget must bound one module tree");
        assert!(file_error.to_string().contains("file snapshot budget"));

        let byte_error = collect_module_compile_input_files(
            &root,
            COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
            super::super::snapshot::MAX_COMPILE_SNAPSHOT_FILES,
            1,
        )
        .expect_err("caller byte budget must bound allocations while reading");
        assert!(
            byte_error.to_string().contains("1-byte limit"),
            "{byte_error}"
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[cfg(unix)]
    #[test]
    fn compile_input_walk_rejects_directory_symlinks_before_traversal() {
        use std::os::unix::fs::symlink;

        let slot = temp_cache_slot("input-symlink-cycle");
        let root = slot.dir.join("project");
        fs::create_dir_all(&root).expect("create project root");
        fs::write(root.join("main.vo"), "package main\n").expect("write source");
        symlink(".", root.join("loop")).expect("create directory cycle");

        let error = collect_compile_input_files(&root, &root, &mut Vec::new())
            .expect_err("directory symlink must be rejected");
        assert!(
            error.to_string().contains("unsupported symbolic link"),
            "{error}"
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[cfg(unix)]
    #[test]
    fn compile_input_walk_rejects_noncyclic_directory_symlink_aliases() {
        use std::os::unix::fs::symlink;

        let slot = temp_cache_slot("input-symlink-aliases");
        let root = slot.dir.join("project");
        let real = root.join("real");
        fs::create_dir_all(&real).expect("create source directory");
        fs::write(real.join("main.vo"), "package main\n").expect("write source");
        symlink("real", root.join("alias")).expect("create directory alias");

        let error = collect_compile_input_files(&root, &root, &mut Vec::new())
            .expect_err("noncyclic directory aliases must also be rejected");
        assert!(
            error.to_string().contains("unsupported symbolic link"),
            "{error}"
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[cfg(unix)]
    #[test]
    fn compile_input_walk_rejects_non_regular_source_entries() {
        use std::os::unix::fs::symlink;

        let slot = temp_cache_slot("input-non-regular");
        let root = slot.dir.join("project");
        fs::create_dir_all(&root).expect("create project root");
        symlink("/dev/null", root.join("blocking.vo"))
            .expect("create source-shaped non-regular entry");

        let error = collect_compile_input_files(&root, &root, &mut Vec::new())
            .expect_err("non-regular source entry must be rejected");
        assert!(
            error.to_string().contains("unsupported symbolic link"),
            "{error}"
        );

        fs::remove_file(root.join("blocking.vo")).expect("remove device symlink");
        fs::write(root.join("actual.vo"), "package actual\n").expect("write target source");
        symlink("actual.vo", root.join("alias.vo")).expect("create regular-file symlink");
        let error = collect_compile_input_files(&root, &root, &mut Vec::new())
            .expect_err("regular-file symlink must be rejected");
        assert!(
            error.to_string().contains("unsupported symbolic link"),
            "{error}"
        );

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[cfg(unix)]
    #[test]
    fn compile_cache_slots_distinguish_arbitrary_unix_file_name_bytes() {
        use std::ffi::OsString;
        use std::os::unix::ffi::OsStringExt;

        let raw = OsString::from_vec(b"main-\xff.vo".to_vec());
        let raw_slot = compile_cache_slot(Path::new("/tmp/project"), Some(&raw));
        let replacement_slot = compile_cache_slot(
            Path::new("/tmp/project"),
            Some(OsStr::new("main-\u{fffd}.vo")),
        );

        assert_ne!(raw_slot.dir, replacement_slot.dir);
    }
}
