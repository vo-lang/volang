use std::collections::{BTreeSet, HashMap};
use std::ffi::OsStr;
#[cfg(unix)]
use std::fs::File;
use std::fs::{self, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use vo_common::stable_hash::StableHasher;
use vo_common::vfs::RealFs;
use vo_module::project::{ProjectContextOptions, ProjectDeps};
use vo_module::schema::lockfile::LockedModule;
use vo_runtime::ext_loader::{NativeExtensionSpec, ABI_FINGERPRINT, ABI_VERSION};
use vo_vm::bytecode::Module;

use super::native::{
    cached_native_extension_spec_is_current_with_workspace, current_target_triple,
    native_build_context_fingerprint,
};
use super::snapshot::CompileInputSnapshot;
use super::{
    CompileError, CompileOutput, COMPILE_CACHE_NATIVE_NAMESPACE, COMPILE_CACHE_SCHEMA_VERSION,
    COMPILE_CACHE_SLOT_NAMESPACE,
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

pub(super) struct CapturedCompileInputs {
    fingerprint: String,
    snapshot: Arc<CompileInputSnapshot>,
}

pub(super) struct CompileInputCapture<'a> {
    pub(super) source_root: &'a Path,
    pub(super) project_root: &'a Path,
    pub(super) mod_cache: &'a Path,
    pub(super) single_file: Option<&'a Path>,
    pub(super) project_deps: &'a ProjectDeps,
    pub(super) replaces: &'a HashMap<String, PathBuf>,
    pub(super) workspace_options: &'a ProjectContextOptions,
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
    let CompileInputCapture {
        source_root,
        project_root,
        mod_cache,
        single_file,
        project_deps,
        replaces,
        workspace_options,
        stdlib_source_fingerprint,
    } = input;
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
    hasher.update_str("compiler_version", env!("CARGO_PKG_VERSION"));
    hasher.update_str("compiler_build_id", env!("VO_COMPILER_BUILD_ID"));
    hasher.update_str("stdlib_source_fingerprint", stdlib_source_fingerprint);
    hasher.update_str("target_triple", current_target_triple());
    hasher.update_str("extension_abi_version", &ABI_VERSION.to_string());
    hasher.update_str(
        "extension_abi_fingerprint",
        &format!("{ABI_FINGERPRINT:#x}"),
    );
    hasher.update_path("source_root", &canonical_source_root);
    hasher.update_path("project_root", &canonical_project_root);
    hasher.update_path("mod_cache_root", &canonical_mod_cache);
    hasher.update_bool("single_file", single_file.is_some());
    if let Some(file_path) = single_file {
        hasher.update_path("entry_file", file_path);
    } else {
        hasher.update_str("entry_file", ".");
    }

    let mut native_rust_dirs = capture_compile_input_tree(
        &mut hasher,
        &mut snapshot,
        "project_root",
        &canonical_project_root,
    )?;
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

    let mut replace_entries = replaces
        .iter()
        .map(|(module, path)| {
            let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
            (module.clone(), canonical)
        })
        .collect::<Vec<_>>();
    replace_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut seen_roots = BTreeSet::new();
    for (module, replace_root) in replace_entries {
        hasher.update_str("replace_module", &module);
        hasher.update_path("replace_root", &replace_root);
        if seen_roots.insert(replace_root.clone()) {
            native_rust_dirs.extend(capture_compile_input_tree(
                &mut hasher,
                &mut snapshot,
                &format!("replace:{module}"),
                &replace_root,
            )?);
        }
    }

    capture_locked_module_inputs(
        &mut hasher,
        &mut snapshot,
        &canonical_mod_cache,
        project_deps,
    )?;

    for rust_dir in native_rust_dirs {
        let module_dir = rust_dir.parent().unwrap_or(&rust_dir);
        hasher.update_path("native_rust_dir", &rust_dir);
        hasher.update_str(
            "native_build_context",
            &native_build_context_fingerprint(module_dir)?,
        );
    }

    Ok(CapturedCompileInputs {
        fingerprint: hasher.finish(),
        snapshot: Arc::new(snapshot),
    })
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

fn capture_compile_input_tree(
    hasher: &mut StableHasher,
    snapshot: &mut CompileInputSnapshot,
    label: &str,
    root: &Path,
) -> Result<BTreeSet<PathBuf>, CompileError> {
    let mut files = Vec::new();
    collect_compile_input_files(root, root, &mut files)?;
    files.sort();
    let mut native_rust_dirs = BTreeSet::new();

    hasher.update_str("tree_label", label);
    hasher.update_path("tree_root", root);
    for rel in files {
        if rel.file_name().is_some_and(|name| name == "Cargo.toml") {
            if let Some(rust_dir) = rel
                .parent()
                .filter(|parent| parent.file_name().is_some_and(|name| name == "rust"))
            {
                let rust_dir = root.join(rust_dir);
                native_rust_dirs.insert(
                    rust_dir
                        .canonicalize()
                        .unwrap_or_else(|_| rust_dir.to_path_buf()),
                );
            }
        }
        let path = root.join(&rel);
        let bytes = snapshot.capture_file(&path)?;
        hasher.update_path("file_path", &rel);
        hasher.update_bytes("file_bytes", bytes);
    }

    Ok(native_rust_dirs)
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
        let artifact_paths = locked_module_artifact_input_paths(locked)?;
        let mut files = Vec::new();
        collect_locked_module_input_files(&root, &root, &artifact_paths, &mut files)?;
        files.sort();

        hasher.update_str("locked_module", locked.path.as_str());
        hasher.update_str("locked_version", &locked.version.to_string());
        hasher.update_path("locked_module_root", &root);
        for rel in files {
            let path = root.join(&rel);
            let bytes = snapshot.capture_file(&path)?;
            hasher.update_path("locked_file_path", &rel);
            hasher.update_bytes("locked_file_bytes", bytes);
        }
    }
    Ok(())
}

fn locked_module_artifact_input_paths(
    locked: &LockedModule,
) -> Result<BTreeSet<PathBuf>, CompileError> {
    locked
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
    artifact_paths: &BTreeSet<PathBuf>,
    out: &mut Vec<PathBuf>,
) -> Result<(), CompileError> {
    let mut walk = CompileInputWalkState::default();
    collect_locked_module_input_files_inner(root, dir, artifact_paths, out, &mut walk, 0)
}

fn collect_locked_module_input_files_inner(
    root: &Path,
    dir: &Path,
    artifact_paths: &BTreeSet<PathBuf>,
    out: &mut Vec<PathBuf>,
    walk: &mut CompileInputWalkState,
    depth: usize,
) -> Result<(), CompileError> {
    let canonical_dir = enter_compile_input_directory(walk, dir, depth, "locked module")?;
    let result = (|| {
        let remaining_entries = walk.entry_limit.saturating_sub(walk.entries);
        let mut entries = fs::read_dir(dir)?
            .take(remaining_entries.saturating_add(1))
            .collect::<Result<Vec<_>, _>>()?;
        if entries.len() > COMPILE_INPUT_MAX_ENTRIES_PER_TREE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "locked module directory at {} exceeds the {}-entry limit",
                    dir.display(),
                    COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
                ),
            )
            .into());
        }
        consume_compile_input_entries(walk, entries.len(), root, "locked module")?;
        entries.sort_by_key(|entry| entry.file_name());
        for entry in entries {
            let path = entry.path();
            if should_skip_compile_input_dir(&path) {
                continue;
            }
            let file_type = entry.file_type()?;
            if file_type.is_symlink() {
                return Err(compile_input_symlink_error(&path, "locked module").into());
            }
            if file_type.is_dir() {
                collect_locked_module_input_files_inner(
                    root,
                    &path,
                    artifact_paths,
                    out,
                    walk,
                    depth.saturating_add(1),
                )?;
                continue;
            }

            let rel = path.strip_prefix(root).unwrap_or(&path).to_path_buf();
            let root_metadata = matches!(
                rel.file_name().and_then(|name| name.to_str()),
                Some("vo.mod")
                    | Some("vo.release.json")
                    | Some(".vo-version")
                    | Some(".vo-source-digest")
            ) && rel
                .parent()
                .is_none_or(|parent| parent.as_os_str().is_empty());
            if path.extension().is_some_and(|extension| extension == "vo")
                || root_metadata
                || artifact_paths.contains(&rel)
            {
                ensure_regular_compile_input(&path, "locked module input")?;
                if out.len() >= COMPILE_INPUT_MAX_FILES_PER_TREE {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "locked module input tree at {} exceeds the {}-file limit",
                            root.display(),
                            COMPILE_INPUT_MAX_FILES_PER_TREE,
                        ),
                    )
                    .into());
                }
                out.push(rel);
            }
        }
        Ok(())
    })();
    walk.ancestors.remove(&canonical_dir);
    result
}

fn collect_compile_input_files(
    root: &Path,
    dir: &Path,
    out: &mut Vec<PathBuf>,
) -> Result<(), CompileError> {
    collect_compile_input_files_matching(
        root,
        dir,
        out,
        is_compile_input_file,
        COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
    )
    .map(|_| ())
}

pub(super) fn collect_module_compile_input_files(
    root: &Path,
    entry_limit: usize,
) -> Result<(Vec<PathBuf>, usize), CompileError> {
    let mut files = Vec::new();
    let entries = collect_compile_input_files_matching(
        root,
        root,
        &mut files,
        is_module_compile_input_file,
        entry_limit,
    )?;
    files.sort();
    Ok((files, entries))
}

fn collect_compile_input_files_matching(
    root: &Path,
    dir: &Path,
    out: &mut Vec<PathBuf>,
    include_file: fn(&Path, &Path) -> bool,
    entry_limit: usize,
) -> Result<usize, CompileError> {
    let mut walk = CompileInputWalkState::with_entry_limit(entry_limit);
    collect_compile_input_files_inner(root, dir, out, &mut walk, 0, include_file)?;
    Ok(walk.entries)
}

fn collect_compile_input_files_inner(
    root: &Path,
    dir: &Path,
    out: &mut Vec<PathBuf>,
    walk: &mut CompileInputWalkState,
    depth: usize,
    include_file: fn(&Path, &Path) -> bool,
) -> Result<(), CompileError> {
    let canonical_dir = enter_compile_input_directory(walk, dir, depth, "compile input")?;
    let result = (|| {
        let remaining_entries = walk.entry_limit.saturating_sub(walk.entries);
        let mut entries = fs::read_dir(dir)?
            .take(remaining_entries.saturating_add(1))
            .collect::<Result<Vec<_>, _>>()?;
        if entries.len() > COMPILE_INPUT_MAX_ENTRIES_PER_TREE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "compile input directory at {} exceeds the {}-entry limit",
                    dir.display(),
                    COMPILE_INPUT_MAX_ENTRIES_PER_TREE,
                ),
            )
            .into());
        }
        consume_compile_input_entries(walk, entries.len(), root, "compile input")?;
        entries.sort_by_key(|e| e.file_name());
        for entry in entries {
            let path = entry.path();
            if should_skip_compile_input_dir(&path) {
                continue;
            }
            let file_type = entry.file_type()?;
            if file_type.is_symlink() {
                return Err(compile_input_symlink_error(&path, "compile input").into());
            }
            if file_type.is_dir() {
                collect_compile_input_files_inner(
                    root,
                    &path,
                    out,
                    walk,
                    depth.saturating_add(1),
                    include_file,
                )?;
                continue;
            }
            if !include_file(root, &path) {
                continue;
            }
            ensure_regular_compile_input(&path, "compile input")?;
            if out.len() >= COMPILE_INPUT_MAX_FILES_PER_TREE {
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
            out.push(path.strip_prefix(root).unwrap_or(&path).to_path_buf());
        }
        Ok(())
    })();
    walk.ancestors.remove(&canonical_dir);
    result
}

struct CompileInputWalkState {
    entries: usize,
    entry_limit: usize,
    ancestors: BTreeSet<PathBuf>,
    canonical_root: Option<PathBuf>,
}

impl CompileInputWalkState {
    fn with_entry_limit(entry_limit: usize) -> Self {
        Self {
            entries: 0,
            entry_limit,
            ancestors: BTreeSet::new(),
            canonical_root: None,
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
) -> Result<PathBuf, CompileError> {
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
    let metadata = fs::symlink_metadata(dir)?;
    if metadata.file_type().is_symlink() {
        return Err(compile_input_symlink_error(dir, description).into());
    }
    if !metadata.file_type().is_dir() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "{description} directory at {} is not a real directory",
                dir.display(),
            ),
        )
        .into());
    }
    let canonical = dir.canonicalize()?;
    let current_metadata = fs::symlink_metadata(dir)?;
    let canonical_metadata = fs::metadata(&canonical)?;
    if current_metadata.file_type().is_symlink()
        || !current_metadata.file_type().is_dir()
        || !same_compile_input_identity(&metadata, &current_metadata)
        || !same_compile_input_identity(&metadata, &canonical_metadata)
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "{description} directory at {} changed identity while it was opened",
                dir.display(),
            ),
        )
        .into());
    }
    if let Some(root) = &walk.canonical_root {
        if !canonical.starts_with(root) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "{description} directory at {} resolves outside tree root {}",
                    dir.display(),
                    root.display(),
                ),
            )
            .into());
        }
    } else {
        walk.canonical_root = Some(canonical.clone());
    }
    if !walk.ancestors.insert(canonical.clone()) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "{description} directory cycle detected at {} (resolves to {})",
                dir.display(),
                canonical.display(),
            ),
        )
        .into());
    }
    Ok(canonical)
}

#[cfg(unix)]
fn same_compile_input_identity(left: &fs::Metadata, right: &fs::Metadata) -> bool {
    use std::os::unix::fs::MetadataExt;

    left.dev() == right.dev() && left.ino() == right.ino()
}

#[cfg(windows)]
fn same_compile_input_identity(left: &fs::Metadata, right: &fs::Metadata) -> bool {
    use std::os::windows::fs::MetadataExt;

    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
    left.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT == 0
        && right.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT == 0
        && left.volume_serial_number() == right.volume_serial_number()
        && left.file_index() == right.file_index()
}

#[cfg(not(any(unix, windows)))]
fn same_compile_input_identity(left: &fs::Metadata, right: &fs::Metadata) -> bool {
    left.file_type() == right.file_type()
        && left.len() == right.len()
        && left.modified().ok() == right.modified().ok()
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

fn ensure_regular_compile_input(path: &Path, description: &str) -> Result<(), CompileError> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_file() {
        return Ok(());
    }
    if metadata.file_type().is_symlink() {
        return Err(compile_input_symlink_error(path, description).into());
    }
    Err(io::Error::new(
        io::ErrorKind::InvalidData,
        format!("{description} at {} is not a regular file", path.display(),),
    )
    .into())
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

fn should_skip_compile_input_dir(path: &Path) -> bool {
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some(".git") | Some(".volang") | Some(".vo-cache") | Some("node_modules") | Some("target")
    )
}

fn is_compile_input_file(root: &Path, path: &Path) -> bool {
    is_module_compile_input_file(root, path)
        || path
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name == "vo.web.json")
        || is_local_native_extension_input_file(root, path)
}

fn is_module_compile_input_file(_root: &Path, path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext == "vo")
        || matches!(
            path.file_name().and_then(|name| name.to_str()),
            Some("vo.mod") | Some("vo.lock") | Some("vo.work")
        )
}

fn is_local_native_extension_input_file(root: &Path, path: &Path) -> bool {
    let rel = path.strip_prefix(root).unwrap_or(path);
    let under_rust_dir = rel.components().any(|component| {
        matches!(
            component,
            std::path::Component::Normal(name) if name == "rust"
        )
    });
    if !under_rust_dir {
        return false;
    }
    !path
        .file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| {
            name.contains(".voabi-")
                || name.ends_with(".vo-abi")
                || name.ends_with(".vo-inputs")
                || name.starts_with(".vo-build-lock-")
        })
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
    workspace_options: &ProjectContextOptions,
) -> Option<CompileOutput> {
    let entry = compile_cache_entry(slot, fingerprint);
    if !entry.dir.is_dir() {
        return None;
    }

    match load_compile_cache_entry(&entry, source_root, fingerprint, workspace_options) {
        Ok(output) => Some(output),
        Err(CacheEntryLoadError::UnavailableArtifacts | CacheEntryLoadError::Corrupt) => {
            // A published entry is immutable. Any malformed payload therefore
            // represents a crashed/foreign write or disk corruption. Artifact
            // paths can also become stale while the source fingerprint stays
            // unchanged (for example after an A -> B -> A native rebuild).
            // Removing either unusable entry lets the fallback compile publish
            // one coherent replacement generation.
            let _ = fs::remove_dir_all(&entry.dir);
            None
        }
    }
}

enum CacheEntryLoadError {
    Corrupt,
    UnavailableArtifacts,
}

fn load_compile_cache_entry(
    entry: &CompileCacheEntry,
    source_root: &Path,
    fingerprint: &str,
    workspace_options: &ProjectContextOptions,
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

    let module_bytes = vo_common_core::serialize::read_vob_file(&entry.module_file)
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
    if !cached_extensions_have_usable_host_artifacts(&extensions, workspace_options) {
        return Err(CacheEntryLoadError::UnavailableArtifacts);
    }
    let locked_modules =
        serde_json::from_slice(&locked_modules_bytes).map_err(|_| CacheEntryLoadError::Corrupt)?;

    Ok(CompileOutput {
        module,
        source_root: source_root.to_path_buf(),
        extensions,
        locked_modules,
    })
}

fn cached_extensions_have_usable_host_artifacts(
    extensions: &[NativeExtensionSpec],
    workspace_options: &ProjectContextOptions,
) -> bool {
    extensions.iter().all(|extension| {
        cached_native_extension_spec_is_current_with_workspace(
            extension,
            &workspace_options.workspace,
        )
    })
}

fn read_bounded_cache_file(path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
    let file = fs::File::open(path)?;
    let max_bytes_u64 = u64::try_from(max_bytes).unwrap_or(u64::MAX);
    if file.metadata()?.len() > max_bytes_u64 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "cache payload at {} exceeds the {}-byte limit",
                path.display(),
                max_bytes,
            ),
        ));
    }
    let mut bytes = Vec::new();
    file.take(max_bytes_u64.saturating_add(1))
        .read_to_end(&mut bytes)?;
    if bytes.len() > max_bytes {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "cache payload at {} exceeds the {}-byte limit",
                path.display(),
                max_bytes,
            ),
        ));
    }
    Ok(bytes)
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
        maintain_cache_slot(slot, entries_root, &entry.dir);
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
        maintain_cache_slot(slot, entries_root, &entry.dir);
        record_test_fingerprint(slot, fingerprint);
    }
}

fn maintain_cache_slot(slot: &CompileCacheSlot, entries_root: &Path, current_entry: &Path) {
    // Schema 6 stores complete immutable entries below `entries/`. Flat files
    // from earlier schemas are unreachable and can be reclaimed after the
    // first successful publication in this slot.
    for legacy_name in ["module.voc", "extensions", "locked_modules", "fingerprint"] {
        let _ = fs::remove_file(slot.dir.join(legacy_name));
    }

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
        let dir = std::env::temp_dir().join(format!("vo-engine-cache-test-{name}-{nonce}"));
        CompileCacheSlot {
            fingerprint_file: dir.join("fingerprint"),
            dir,
        }
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
    fn cached_published_native_artifacts_do_not_need_local_build_markers() {
        let slot = temp_cache_slot("published-native-artifact");
        let native_path = slot.dir.join("artifacts/libdemo.dylib");
        fs::create_dir_all(native_path.parent().unwrap()).unwrap();
        fs::write(&native_path, b"published native artifact").unwrap();
        let spec = native_spec(native_path.clone());

        assert!(cached_extensions_have_usable_host_artifacts(
            std::slice::from_ref(&spec),
            &ProjectContextOptions::default(),
        ));
        fs::remove_file(native_path).unwrap();
        assert!(!cached_extensions_have_usable_host_artifacts(
            &[spec],
            &ProjectContextOptions::default(),
        ));

        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn locked_module_input_collection_includes_nested_target_artifacts() {
        let slot = temp_cache_slot("nested-locked-artifact");
        let root = slot.dir.join("module");
        let artifact = vo_module::schema::lockfile::LockedArtifact {
            id: vo_module::identity::ArtifactId {
                kind: "extension-native".to_string(),
                target: current_target_triple().to_string(),
                name: "libdemo.bin".to_string(),
            },
            size: 4,
            digest: vo_module::digest::Digest::from_sha256(b"demo"),
        };
        let locked = LockedModule {
            path: vo_module::identity::ModulePath::parse("github.com/acme/demo").unwrap(),
            version: vo_module::version::ExactVersion::parse("v1.0.0").unwrap(),
            vo: vo_module::version::ToolchainConstraint::parse("^0.1.0").unwrap(),
            commit: "1111111111111111111111111111111111111111".to_string(),
            release_manifest: vo_module::digest::Digest::parse(
                "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            )
            .unwrap(),
            source: vo_module::digest::Digest::parse(
                "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            )
            .unwrap(),
            deps: Vec::new(),
            artifacts: vec![artifact],
        };
        let relative_artifact =
            vo_module::artifact::artifact_relative_path(&locked.artifacts[0].id).unwrap();
        let artifact_path = root.join(&relative_artifact);
        fs::create_dir_all(artifact_path.parent().unwrap()).unwrap();
        fs::write(&artifact_path, b"demo").unwrap();

        let artifact_paths = locked_module_artifact_input_paths(&locked).unwrap();
        let mut files = Vec::new();
        collect_locked_module_input_files(&root, &root, &artifact_paths, &mut files).unwrap();

        assert_eq!(files, vec![relative_artifact]);
        let _ = fs::remove_dir_all(&slot.dir);
    }

    #[test]
    fn cached_local_load_copies_need_current_build_markers() {
        let spec = native_spec(PathBuf::from(
            "rust/target/debug/libdemo.voabi-1-deadbeef.dylib",
        ));

        assert!(!cached_extensions_have_usable_host_artifacts(
            &[spec],
            &ProjectContextOptions::default(),
        ));
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
    fn unavailable_artifact_entry_is_removed_and_can_be_republished() {
        let slot = temp_cache_slot("unavailable-artifact");
        let source_root = slot.dir.join("sources");
        let mut output = crate::compile_source_at("package main\nfunc main() {}\n", &source_root)
            .expect("compile cache fixture");
        output.extensions = vec![native_spec(slot.dir.join("missing/libdemo.dylib"))];

        let fingerprint = "same-source-fingerprint";
        save_compile_cache(&slot, fingerprint, &output);
        let entry = compile_cache_entry(&slot, fingerprint);
        assert!(entry.dir.is_dir());
        assert!(try_load_cache(&slot, &source_root, fingerprint).is_none());
        assert!(
            !entry.dir.exists(),
            "an immutable entry with stale artifact paths must be removed"
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
        for legacy in ["module.voc", "extensions", "locked_modules"] {
            fs::write(slot.dir.join(legacy), b"legacy").expect("write legacy cache file");
        }
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
        for legacy in ["module.voc", "extensions", "locked_modules"] {
            assert!(!slot.dir.join(legacy).exists());
        }

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
        let replaces = HashMap::new();
        let workspace_options =
            ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled);

        let fingerprint = |stdlib_identity: &str| {
            capture_compile_inputs(CompileInputCapture {
                source_root: &project_root,
                project_root: &project_root,
                mod_cache: &mod_cache,
                single_file: Some(&entry),
                project_deps: &project_deps,
                replaces: &replaces,
                workspace_options: &workspace_options,
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
        let replaces = HashMap::new();
        let workspace_options =
            ProjectContextOptions::new(vo_module::workspace::WorkspaceDiscovery::Disabled);

        let fingerprint = |captured_source_root: &Path| {
            capture_compile_inputs(CompileInputCapture {
                source_root: captured_source_root,
                project_root: &project_root,
                mod_cache: &mod_cache,
                single_file: Some(&entry),
                project_deps: &project_deps,
                replaces: &replaces,
                workspace_options: &workspace_options,
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
    fn module_compile_input_walk_honors_caller_entry_budget() {
        let slot = temp_cache_slot("module-input-budget");
        let root = slot.dir.join("project");
        fs::create_dir_all(&root).expect("create project root");
        fs::write(root.join("a.vo"), "package a\n").expect("write source a");
        fs::write(root.join("b.vo"), "package b\n").expect("write source b");

        let error = collect_module_compile_input_files(&root, 1)
            .expect_err("caller entry budget must bound one module tree");
        assert!(error.to_string().contains("entry limit"), "{error}");

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
