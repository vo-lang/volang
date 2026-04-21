use std::collections::{BTreeSet, HashMap};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use vo_common::stable_hash::StableHasher;
use vo_module::project::ProjectDeps;
use vo_module::schema::lockfile::LockedModule;
use vo_runtime::ext_loader::NativeExtensionSpec;
use vo_vm::bytecode::Module;

use super::native::current_target_triple;
use super::{
    CompileError, CompileOutput, COMPILE_CACHE_NATIVE_NAMESPACE, COMPILE_CACHE_SCHEMA_VERSION,
    COMPILE_CACHE_SLOT_NAMESPACE,
};

#[derive(serde::Serialize, serde::Deserialize)]
struct CachedNativeExtensionSpec {
    name: String,
    native_path: PathBuf,
    manifest_path: PathBuf,
}

impl From<&NativeExtensionSpec> for CachedNativeExtensionSpec {
    fn from(spec: &NativeExtensionSpec) -> Self {
        Self {
            name: spec.name.clone(),
            native_path: spec.native_path.clone(),
            manifest_path: spec.manifest_path.clone(),
        }
    }
}

impl From<CachedNativeExtensionSpec> for NativeExtensionSpec {
    fn from(spec: CachedNativeExtensionSpec) -> Self {
        NativeExtensionSpec::new(spec.name, spec.native_path, spec.manifest_path)
    }
}

pub(super) struct CompileCacheSlot {
    pub(super) dir: PathBuf,
    pub(super) fingerprint_file: PathBuf,
    pub(super) module_file: PathBuf,
    pub(super) extensions_file: PathBuf,
    pub(super) locked_modules_file: PathBuf,
}

pub(super) fn compile_cache_slot(root: &Path, single_file: Option<&OsStr>) -> CompileCacheSlot {
    let mut slot_hasher = StableHasher::new(COMPILE_CACHE_SLOT_NAMESPACE);
    if let Some(file_name) = single_file {
        slot_hasher.update_str("entry_kind", "file");
        slot_hasher.update_str("entry_name", &file_name.to_string_lossy());
    } else {
        slot_hasher.update_str("entry_kind", "dir");
        slot_hasher.update_str("entry_name", ".");
    }
    let slot_id = slot_hasher.finish_suffix();
    let dir = root
        .join(".vo-cache")
        .join("compile")
        .join("native")
        .join(slot_id);
    CompileCacheSlot {
        fingerprint_file: dir.join("fingerprint"),
        module_file: dir.join("module.voc"),
        extensions_file: dir.join("extensions"),
        locked_modules_file: dir.join("locked_modules"),
        dir,
    }
}

pub(super) fn compute_compile_cache_fingerprint(
    source_root: &Path,
    project_root: &Path,
    mod_cache: &Path,
    single_file: Option<&Path>,
    project_deps: &ProjectDeps,
    replaces: &HashMap<String, PathBuf>,
) -> Result<String, CompileError> {
    let canonical_source_root = source_root
        .canonicalize()
        .unwrap_or_else(|_| source_root.to_path_buf());
    let canonical_project_root = project_root
        .canonicalize()
        .unwrap_or_else(|_| project_root.to_path_buf());
    let canonical_mod_cache = mod_cache
        .canonicalize()
        .unwrap_or_else(|_| mod_cache.to_path_buf());
    let mut hasher = StableHasher::new(COMPILE_CACHE_NATIVE_NAMESPACE);
    hasher.update_str("schema", COMPILE_CACHE_SCHEMA_VERSION);
    hasher.update_str("compiler_version", env!("CARGO_PKG_VERSION"));
    hasher.update_str("compiler_build_id", env!("VO_COMPILER_BUILD_ID"));
    hasher.update_str("target_triple", current_target_triple());
    hasher.update_path("source_root", &canonical_source_root);
    hasher.update_path("project_root", &canonical_project_root);
    hasher.update_path("mod_cache_root", &canonical_mod_cache);
    hasher.update_bool("single_file", single_file.is_some());
    if let Some(file_path) = single_file {
        hasher.update_path("entry_file", file_path);
    } else {
        hasher.update_str("entry_file", ".");
    }

    hash_compile_input_tree(&mut hasher, "project_root", &canonical_project_root)?;
    hash_project_deps(&mut hasher, project_deps);

    if let Some(workfile_path) = vo_module::workspace::discover_workfile(&canonical_project_root) {
        let canonical_workfile = workfile_path.canonicalize().unwrap_or(workfile_path);
        hasher.update_path("workspace_file", &canonical_workfile);
        hasher.update_bytes("workspace_file_bytes", &fs::read(&canonical_workfile)?);
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
            hash_compile_input_tree(&mut hasher, &format!("replace:{module}"), &replace_root)?;
        }
    }

    Ok(hasher.finish())
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
            .map(|mod_file| mod_file.render())
            .unwrap_or_default(),
    );
    hasher.update_str(
        "project_deps_lock_file",
        &project_deps
            .lock_file()
            .map(|lock_file| lock_file.render())
            .unwrap_or_default(),
    );
}

fn hash_compile_input_tree(
    hasher: &mut StableHasher,
    label: &str,
    root: &Path,
) -> Result<(), CompileError> {
    let mut files = Vec::new();
    collect_compile_input_files(root, root, &mut files)?;
    files.sort();

    hasher.update_str("tree_label", label);
    hasher.update_path("tree_root", root);
    for rel in files {
        hasher.update_path("file_path", &rel);
        hasher.update_bytes("file_bytes", &fs::read(root.join(&rel))?);
    }

    Ok(())
}

fn collect_compile_input_files(
    root: &Path,
    dir: &Path,
    out: &mut Vec<PathBuf>,
) -> Result<(), CompileError> {
    let mut entries = fs::read_dir(dir)?.collect::<Result<Vec<_>, _>>()?;
    entries.sort_by_key(|e| e.file_name());
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            if should_skip_compile_input_dir(&path) {
                continue;
            }
            collect_compile_input_files(root, &path, out)?;
            continue;
        }
        if !is_compile_input_file(&path) {
            continue;
        }
        out.push(path.strip_prefix(root).unwrap_or(&path).to_path_buf());
    }
    Ok(())
}

fn should_skip_compile_input_dir(path: &Path) -> bool {
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some(".git") | Some(".vo-cache") | Some("node_modules") | Some("target")
    )
}

fn is_compile_input_file(path: &Path) -> bool {
    if path.extension().map(|ext| ext == "vo").unwrap_or(false) {
        return true;
    }
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some("vo.mod") | Some("vo.lock") | Some("vo.ext.toml") | Some("vo.work")
    )
}

pub(super) fn try_load_cache(
    slot: &CompileCacheSlot,
    source_root: &Path,
    fingerprint: &str,
) -> Option<CompileOutput> {
    let cached_fingerprint = fs::read_to_string(&slot.fingerprint_file).ok()?;
    if cached_fingerprint.trim() != fingerprint {
        return None;
    }

    let bytes = fs::read(&slot.module_file).ok()?;
    let module = Module::deserialize(&bytes).ok()?;
    let extensions = load_extensions(&slot.extensions_file)?;
    let locked_modules = load_locked_modules(&slot.locked_modules_file)?;

    Some(CompileOutput {
        module,
        source_root: source_root.to_path_buf(),
        extensions,
        locked_modules,
    })
}

pub(super) fn save_compile_cache(
    slot: &CompileCacheSlot,
    fingerprint: &str,
    output: &CompileOutput,
) {
    let ok = fs::create_dir_all(&slot.dir).is_ok()
        && fs::write(&slot.module_file, output.module.serialize()).is_ok()
        && save_extensions(&slot.extensions_file, &output.extensions)
        && save_locked_modules(&slot.locked_modules_file, &output.locked_modules);
    if ok {
        let _ = fs::write(&slot.fingerprint_file, format!("{fingerprint}\n"));
    }
}

fn save_extensions(path: &Path, extensions: &[NativeExtensionSpec]) -> bool {
    let cached = extensions
        .iter()
        .map(CachedNativeExtensionSpec::from)
        .collect::<Vec<_>>();
    match serde_json::to_vec(&cached) {
        Ok(bytes) => fs::write(path, bytes).is_ok(),
        Err(_) => false,
    }
}

fn load_extensions(path: &Path) -> Option<Vec<NativeExtensionSpec>> {
    match fs::read(path) {
        Ok(bytes) => serde_json::from_slice::<Vec<CachedNativeExtensionSpec>>(&bytes)
            .ok()
            .map(|extensions| {
                extensions
                    .into_iter()
                    .map(NativeExtensionSpec::from)
                    .collect()
            }),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Some(Vec::new()),
        Err(_) => None,
    }
}

fn save_locked_modules(path: &Path, locked_modules: &[LockedModule]) -> bool {
    match serde_json::to_vec(locked_modules) {
        Ok(bytes) => fs::write(path, bytes).is_ok(),
        Err(_) => false,
    }
}

fn load_locked_modules(path: &Path) -> Option<Vec<LockedModule>> {
    match fs::read(path) {
        Ok(bytes) => serde_json::from_slice(&bytes).ok(),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Some(Vec::new()),
        Err(_) => None,
    }
}
