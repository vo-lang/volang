use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ffi::OsString;
use std::fs::{self, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock, Weak};

use serde::Deserialize;
use vo_common::{
    stable_hash::StableHasher,
    vfs::{RealFs, MAX_TEXT_FILE_BYTES},
};
use vo_module::cache::validate::InstalledModuleError;
use vo_module::ext_manifest::{parse_ext_manifest_content, ExtensionManifest};
use vo_module::readiness::{
    check_materialized_modules_readiness, check_project_readiness, ReadinessFailure, ReadyModule,
};
use vo_module::schema::lockfile::LockedModule;
use vo_module::schema::modfile::ModFile;
use vo_runtime::ext_loader::{ExtensionLoader, NativeExtensionSpec, ABI_FINGERPRINT, ABI_VERSION};

use super::cache::collect_module_compile_input_files;
#[cfg(test)]
use super::CompileError;
use super::{
    emit_compile_log, CompileLogRecord, ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage,
};

const NATIVE_EXTENSION_ABI_MARKER_FORMAT_VERSION: u32 = 2;
const NATIVE_EXTENSION_INPUT_MARKER_FORMAT_VERSION: u32 = 1;
const NATIVE_EXTENSION_BUILD_ATTEMPTS: usize = 3;
const NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES: usize = 64 * 1024 * 1024;
const NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES: usize = 256 * 1024 * 1024;
const NATIVE_EXTENSION_MAX_INPUT_FILES: usize = 100_000;
const NATIVE_EXTENSION_MAX_INPUT_ENTRIES: usize = 200_000;
const NATIVE_EXTENSION_MAX_INPUT_DIRECTORY_DEPTH: usize = 256;
const NATIVE_EXTENSION_MAX_ARTIFACT_BYTES: usize = 256 * 1024 * 1024;
const NATIVE_EXTENSION_MAX_MARKER_BYTES: usize = 4 * 1024;
const NATIVE_EXTENSION_MAX_CARGO_METADATA_BYTES: usize = 32 * 1024 * 1024;
const NATIVE_EXTENSION_MAX_CARGO_BUILD_OUTPUT_BYTES: usize = 64 * 1024 * 1024;
const NATIVE_EXTENSION_FFI_SOURCE_FINGERPRINT_ENV: &str = "VO_FFI_SOURCE_FINGERPRINT";
const NATIVE_EXTENSION_RUSTC_FINGERPRINT_CFG: &str = "vo_ffi_input_fingerprint";
static NATIVE_MARKER_TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);
static NATIVE_BUILD_LOCKS: OnceLock<Mutex<HashMap<PathBuf, Weak<Mutex<()>>>>> = OnceLock::new();

#[cfg(test)]
pub(super) fn validate_locked_modules_installed(
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<(), CompileError> {
    check_frozen_dependency_readiness(locked_modules, mod_root)
        .map(|_| ())
        .map_err(CompileError::ModuleSystem)?;
    Ok(())
}

pub(super) fn check_frozen_dependency_readiness(
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<Vec<ReadyModule>, ModuleSystemError> {
    let fs = RealFs::new(mod_root);
    check_frozen_dependency_readiness_with_fs(&fs, locked_modules)
}

pub(super) fn check_frozen_dependency_readiness_with_fs<F: vo_common::vfs::FileSystem>(
    fs: &F,
    locked_modules: &[LockedModule],
) -> Result<Vec<ReadyModule>, ModuleSystemError> {
    check_project_readiness(fs, locked_modules, current_target_triple())
        .map_err(module_readiness_failure_to_module_system)
}

pub(super) fn check_materialized_dependency_readiness(
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<Vec<ReadyModule>, ModuleSystemError> {
    let fs = RealFs::new(mod_root);
    check_materialized_dependency_readiness_with_fs(&fs, locked_modules)
}

/// Validate source and artifact state for the subset authorized by an already
/// validated `ProjectDeps` context. Workspace overrides can remove registry
/// nodes from materialization while the complete root lock remains unchanged.
pub(super) fn check_materialized_dependency_readiness_with_fs<F: vo_common::vfs::FileSystem>(
    fs: &F,
    locked_modules: &[LockedModule],
) -> Result<Vec<ReadyModule>, ModuleSystemError> {
    check_materialized_modules_readiness(fs, locked_modules, current_target_triple())
        .map_err(module_readiness_failure_to_module_system)
}

fn installed_module_error_to_module_system(e: InstalledModuleError) -> ModuleSystemError {
    use vo_module::cache::validate::InstalledModuleErrorKind;
    let (kind, detail) = match e.kind.as_ref() {
        InstalledModuleErrorKind::Missing { .. } => (
            ModuleSystemErrorKind::Missing,
            format!("{}: frozen builds do not auto-install dependencies", e,),
        ),
        InstalledModuleErrorKind::Mismatch { .. } => (
            ModuleSystemErrorKind::Mismatch,
            format!("{}: frozen builds do not auto-install dependencies", e,),
        ),
        InstalledModuleErrorKind::ParseFailed { .. } => {
            (ModuleSystemErrorKind::ParseFailed, e.to_string())
        }
        InstalledModuleErrorKind::ValidationFailed { .. }
        | InstalledModuleErrorKind::LockedModuleMismatch { .. } => {
            (ModuleSystemErrorKind::ValidationFailed, e.to_string())
        }
    };
    ModuleSystemError::new(ModuleSystemStage::CachedModule, kind, detail)
        .with_module_version(e.module, e.version.to_string())
}

pub(super) fn module_readiness_failure_to_module_system(
    failure: ReadinessFailure,
) -> ModuleSystemError {
    match failure {
        ReadinessFailure::LockedGraphInvalid { error } => ModuleSystemError::new(
            ModuleSystemStage::LockFile,
            ModuleSystemErrorKind::ValidationFailed,
            error.to_string(),
        ),
        ReadinessFailure::SourceNotReady { error } => {
            installed_module_error_to_module_system(*error)
        }
        ReadinessFailure::UnsupportedNativeTarget {
            module,
            version,
            target,
            manifest_path,
        } => ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::Missing,
            format!(
                "vo.mod does not declare extension-native support for target {} in {}@{}",
                target, module, version,
            ),
        )
        .with_module_version(module, version)
        .with_path(&manifest_path),
        ReadinessFailure::ArtifactResolutionFailed {
            module,
            version,
            manifest_path,
            error,
        } => match *error {
            vo_module::Error::MissingLockedArtifact { detail, .. } => ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::Missing,
                detail,
            )
            .with_module_version(module, version)
            .with_path(&manifest_path),
            error => ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                error.to_string(),
            )
            .with_module_version(module, version)
            .with_path(&manifest_path),
        },
        ReadinessFailure::ArtifactNotReady {
            artifact_path,
            error,
            ..
        } => installed_module_error_to_module_system(*error)
            .with_stage(ModuleSystemStage::NativeExtension)
            .with_path(&artifact_path),
    }
}

pub(super) fn prepare_native_extension_specs_for_frozen_build(
    manifests: &[ExtensionManifest],
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<Vec<NativeExtensionSpec>, ModuleSystemError> {
    prepare_native_extension_specs_for_frozen_build_with_workspace(
        manifests,
        locked_modules,
        mod_root,
        &vo_module::workspace::workspace_discovery_from_environment(),
    )
}

pub(super) fn prepare_native_extension_specs_for_frozen_build_with_workspace(
    manifests: &[ExtensionManifest],
    locked_modules: &[LockedModule],
    mod_root: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<Vec<NativeExtensionSpec>, ModuleSystemError> {
    let ready_modules = check_frozen_dependency_readiness(locked_modules, mod_root)?;
    prepare_native_extension_specs_with_readiness_and_workspace(
        manifests,
        &ready_modules,
        mod_root,
        workspace_discovery,
    )
}

#[cfg(test)]
pub(super) fn prepare_native_extension_specs_with_readiness(
    manifests: &[ExtensionManifest],
    ready_modules: &[ReadyModule],
    mod_root: &Path,
) -> Result<Vec<NativeExtensionSpec>, ModuleSystemError> {
    prepare_native_extension_specs_with_readiness_and_workspace(
        manifests,
        ready_modules,
        mod_root,
        &vo_module::workspace::workspace_discovery_from_environment(),
    )
}

pub(super) fn prepare_native_extension_specs_with_readiness_and_workspace(
    manifests: &[ExtensionManifest],
    ready_modules: &[ReadyModule],
    mod_root: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<Vec<NativeExtensionSpec>, ModuleSystemError> {
    use std::collections::BTreeSet;

    let mod_root = mod_root
        .canonicalize()
        .unwrap_or_else(|_| mod_root.to_path_buf());
    let mut seen_module_dirs = BTreeSet::new();
    let mut specs = Vec::new();
    let native_manifests = manifests
        .iter()
        .filter(|manifest| manifest.native.is_some())
        .collect::<Vec<_>>();

    for manifest in &native_manifests {
        let module_dir = extension_manifest_module_dir(manifest)?;
        if !seen_module_dirs.insert(module_dir) {
            continue;
        }
        let resolved =
            prepare_extension_spec(manifest, ready_modules, &mod_root, workspace_discovery)?;
        specs.push(resolved);
    }

    Ok(specs)
}

pub(super) fn current_target_triple() -> &'static str {
    env!("VO_TARGET_TRIPLE")
}

fn native_extension_spec(
    manifest: &ExtensionManifest,
    module_owner: &str,
    native_path: PathBuf,
) -> NativeExtensionSpec {
    NativeExtensionSpec::new(
        manifest.name.clone(),
        module_owner,
        native_path,
        manifest.manifest_path.clone(),
    )
}

fn local_extension_module_owner(manifest: &ExtensionManifest) -> Result<String, ModuleSystemError> {
    module_owner_from_manifest_path(&manifest.manifest_path)
}

fn module_owner_from_manifest_path(manifest_path: &Path) -> Result<String, ModuleSystemError> {
    let content = read_bounded_text_file(
        manifest_path,
        MAX_TEXT_FILE_BYTES,
        "native extension manifest",
    )
    .map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            error.to_string(),
        )
        .with_path(manifest_path)
    })?;
    let mod_file = ModFile::parse(&content).map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ParseFailed,
            format!(
                "failed to parse native extension module identity from {}: {error}",
                manifest_path.display()
            ),
        )
        .with_path(manifest_path)
    })?;
    let module = mod_file.module.as_github().ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "native extensions require a canonical published module path in {}",
                manifest_path.display()
            ),
        )
        .with_path(manifest_path)
    })?;
    Ok(module.as_str().to_string())
}

fn manifest_local_native_path(
    manifest: &ExtensionManifest,
    module_dir: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    manifest
        .resolve_local_native_path(module_dir)
        .map_err(|error| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                error.to_string(),
            )
            .with_path(&manifest.manifest_path)
        })?
        .ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "native extension manifest does not declare [extension.native]: {}",
                    manifest.manifest_path.display(),
                ),
            )
            .with_path(&manifest.manifest_path)
        })
}

fn extension_manifest_module_dir(
    manifest: &ExtensionManifest,
) -> Result<PathBuf, ModuleSystemError> {
    let module_dir = manifest.manifest_path.parent().ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "extension manifest path has no parent: {}",
                manifest.manifest_path.display(),
            ),
        )
        .with_path(&manifest.manifest_path)
    })?;
    Ok(module_dir
        .canonicalize()
        .unwrap_or_else(|_| module_dir.to_path_buf()))
}

fn prepare_extension_spec(
    manifest: &ExtensionManifest,
    ready_modules: &[ReadyModule],
    mod_root: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<NativeExtensionSpec, ModuleSystemError> {
    let module_dir = extension_manifest_module_dir(manifest)?;
    if !module_dir.starts_with(mod_root) {
        let native_path = manifest_local_native_path(manifest, &module_dir)?;
        let load_path = prepare_local_native_extension_load_path(
            &module_dir,
            &native_path,
            workspace_discovery,
        )?;
        let module_owner = local_extension_module_owner(manifest)?;
        return Ok(native_extension_spec(manifest, &module_owner, load_path));
    }

    let ready = ready_module_for_cached_extension(&module_dir, mod_root, ready_modules)?;
    prepare_cached_extension_spec(manifest, ready, &module_dir)
}

fn prepare_local_native_extension_load_path(
    module_dir: &Path,
    native_path: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<PathBuf, ModuleSystemError> {
    let rust_manifest = module_dir.join("rust").join("Cargo.toml");
    if !rust_manifest.exists() {
        return Ok(native_path.to_path_buf());
    }
    ensure_local_native_extension_built_with_workspace(module_dir, native_path, workspace_discovery)
}

fn prepare_cached_extension_spec(
    manifest: &ExtensionManifest,
    ready: &ReadyModule,
    module_dir: &Path,
) -> Result<NativeExtensionSpec, ModuleSystemError> {
    let artifact = ready
        .artifacts()
        .iter()
        .find(|artifact| artifact.id().kind == "extension-native")
        .ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::Missing,
                format!(
                    "vo.mod does not declare extension-native support for target {} in {}@{}",
                    current_target_triple(),
                    ready.module(),
                    ready.version(),
                ),
            )
            .with_module_version(ready.module().as_str(), ready.version().to_string())
            .with_path(&manifest.manifest_path)
        })?;
    Ok(native_extension_spec(
        manifest,
        ready.module().as_str(),
        module_dir.join(artifact.cache_relative_path()),
    ))
}

fn ready_module_for_cached_extension<'a>(
    module_dir: &Path,
    mod_root: &Path,
    ready_modules: &'a [ReadyModule],
) -> Result<&'a ReadyModule, ModuleSystemError> {
    if let Some((module_path, version)) =
        vo_module::cache::layout::module_identity_from_cache_dir(mod_root, module_dir)
    {
        if let Some(ready) = ready_modules
            .iter()
            .find(|ready| ready.module() == &module_path && ready.version() == &version)
        {
            return Ok(ready);
        }
    }
    let (module_path, version) = vo_module::cache::layout::module_identity_from_cache_dir(
        mod_root, module_dir,
    )
    .ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "failed to infer module path for cached extension at {}",
                module_dir.display(),
            ),
        )
        .with_path(module_dir)
    })?;
    Err(ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::ValidationFailed,
        format!(
            "missing locked module metadata for cached extension {}@{}",
            module_path, version,
        ),
    )
    .with_path(module_dir))
}

#[derive(Debug, Deserialize)]
struct CargoMetadataDocument {
    packages: Vec<CargoMetadataPackage>,
    workspace_root: PathBuf,
    target_directory: PathBuf,
    resolve: Option<CargoMetadataResolve>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadataPackage {
    id: String,
    name: String,
    manifest_path: PathBuf,
    source: Option<String>,
    targets: Vec<CargoMetadataTarget>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadataTarget {
    name: String,
    kind: Vec<String>,
    crate_types: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadataResolve {
    nodes: Vec<CargoMetadataNode>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadataNode {
    id: String,
    deps: Vec<CargoMetadataDependency>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadataDependency {
    pkg: String,
}

#[derive(Debug, Clone)]
struct NativeCargoContext {
    rust_dir: PathBuf,
    manifest_path: PathBuf,
    workspace_root: PathBuf,
    lock_path: PathBuf,
    target_directory: PathBuf,
    root_package_id: String,
    root_package_name: String,
    root_target_name: String,
    local_package_roots: Vec<PathBuf>,
    patch_configs: Vec<String>,
    metadata_digest: String,
}

fn normalized_workspace_discovery(
    module_dir: &Path,
    discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<vo_module::workspace::WorkspaceDiscovery, ModuleSystemError> {
    match discovery {
        vo_module::workspace::WorkspaceDiscovery::Auto => {
            Ok(vo_module::workspace::WorkspaceDiscovery::Auto)
        }
        vo_module::workspace::WorkspaceDiscovery::Disabled => {
            Ok(vo_module::workspace::WorkspaceDiscovery::Disabled)
        }
        vo_module::workspace::WorkspaceDiscovery::Explicit(path) => {
            let selected = if path.is_absolute() {
                path.clone()
            } else {
                module_dir.join(path)
            };
            let selected = selected.canonicalize().map_err(|error| {
                ModuleSystemError::new(
                    ModuleSystemStage::Workspace,
                    ModuleSystemErrorKind::ReadFailed,
                    format!(
                        "failed to resolve explicit native extension workspace {}: {}",
                        selected.display(),
                        error,
                    ),
                )
                .with_path(&selected)
            })?;
            let metadata = fs::symlink_metadata(&selected)
                .map_err(|error| native_build_input_read_error(&selected, error))?;
            if metadata.file_type().is_symlink() || !metadata.file_type().is_file() {
                return Err(native_build_input_read_error(
                    &selected,
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "explicit native extension workspace must be a regular file",
                    ),
                ));
            }
            Ok(vo_module::workspace::WorkspaceDiscovery::Explicit(selected))
        }
    }
}

fn apply_workspace_discovery_to_command(
    command: &mut std::process::Command,
    discovery: &vo_module::workspace::WorkspaceDiscovery,
) {
    match discovery {
        vo_module::workspace::WorkspaceDiscovery::Auto => {
            command.env_remove("VOWORK");
        }
        vo_module::workspace::WorkspaceDiscovery::Disabled => {
            command.env("VOWORK", "off");
        }
        vo_module::workspace::WorkspaceDiscovery::Explicit(path) => {
            command.env("VOWORK", path);
        }
    }
}

fn inspect_native_cargo_context(
    module_dir: &Path,
    native_path: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<NativeCargoContext, ModuleSystemError> {
    let rust_dir = module_dir
        .join("rust")
        .canonicalize()
        .map_err(|error| native_build_input_read_error(&module_dir.join("rust"), error))?;
    let manifest_path = rust_dir
        .join("Cargo.toml")
        .canonicalize()
        .map_err(|error| native_build_input_read_error(&rust_dir.join("Cargo.toml"), error))?;
    let located_workspace_root =
        locate_native_cargo_workspace_root(&rust_dir, &manifest_path, workspace_discovery)?;
    let located_lock_path = located_workspace_root.join("Cargo.lock");
    let lock_bytes = read_bounded_file(
        &located_lock_path,
        MAX_TEXT_FILE_BYTES,
        "Cargo workspace lockfile",
    )
    .map_err(|error| {
        let kind = if error.kind() == std::io::ErrorKind::NotFound {
            ModuleSystemErrorKind::Missing
        } else {
            ModuleSystemErrorKind::ReadFailed
        };
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            kind,
            format!(
                "native extension builds require the Cargo workspace lockfile at {}: {}",
                located_lock_path.display(),
                error,
            ),
        )
        .with_path(&located_lock_path)
    })?;
    let patch_configs =
        if native_cargo_uses_volang_git_source(&rust_dir, &located_workspace_root, &lock_bytes)? {
            local_volang_cargo_patch_configs()
        } else {
            Vec::new()
        };
    let mut command = std::process::Command::new("cargo");
    command
        .arg("metadata")
        .args(["--format-version", "1"])
        .arg("--locked")
        .arg("--manifest-path")
        .arg(&manifest_path);
    for config in &patch_configs {
        command.arg("--config").arg(config);
    }
    let declared_target_directory = declared_native_cargo_target_directory(native_path)?;
    command
        .current_dir(&rust_dir)
        .env("CARGO_TARGET_DIR", &declared_target_directory)
        .env_remove("CARGO_BUILD_TARGET");
    apply_workspace_discovery_to_command(&mut command, workspace_discovery);
    let output =
        run_command_with_bounded_output(&mut command, NATIVE_EXTENSION_MAX_CARGO_METADATA_BYTES)
            .map_err(|error| {
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::BuildFailed,
                    format!("failed to inspect native extension Cargo graph: {error}"),
                )
                .with_path(&manifest_path)
            })?;
    if output.stdout.len() > NATIVE_EXTENSION_MAX_CARGO_METADATA_BYTES
        || output.stderr.len() > NATIVE_EXTENSION_MAX_CARGO_METADATA_BYTES
    {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "cargo metadata output for {} exceeds the {}-byte limit",
                manifest_path.display(),
                NATIVE_EXTENSION_MAX_CARGO_METADATA_BYTES,
            ),
        )
        .with_path(&manifest_path));
    }
    if !output.status.success() {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "cargo metadata --locked failed for {}: {}",
                rust_dir.display(),
                String::from_utf8_lossy(&output.stderr),
            ),
        )
        .with_path(&manifest_path));
    }
    let metadata: CargoMetadataDocument =
        serde_json::from_slice(&output.stdout).map_err(|error| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "cargo metadata returned invalid JSON for {}: {}",
                    manifest_path.display(),
                    error,
                ),
            )
            .with_path(&manifest_path)
        })?;

    let root_package = metadata
        .packages
        .iter()
        .find(|package| {
            package
                .manifest_path
                .canonicalize()
                .is_ok_and(|path| path == manifest_path)
        })
        .ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "cargo metadata did not identify the native extension package at {}",
                    manifest_path.display(),
                ),
            )
            .with_path(&manifest_path)
        })?;
    let mut cdylib_targets = root_package
        .targets
        .iter()
        .filter(|target| {
            target.kind.iter().any(|kind| kind == "cdylib")
                || target.crate_types.iter().any(|kind| kind == "cdylib")
        })
        .collect::<Vec<_>>();
    if cdylib_targets.len() != 1 {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "native extension Cargo package {} must expose exactly one cdylib target; found {}",
                root_package.name,
                cdylib_targets.len(),
            ),
        )
        .with_path(&manifest_path));
    }
    let root_target_name = cdylib_targets
        .pop()
        .expect("exactly one cdylib target was checked")
        .name
        .clone();

    let resolve = metadata.resolve.as_ref().ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            "cargo metadata omitted the resolved native extension dependency graph",
        )
        .with_path(&manifest_path)
    })?;
    let nodes = resolve
        .nodes
        .iter()
        .map(|node| (node.id.as_str(), node))
        .collect::<BTreeMap<_, _>>();
    let mut reachable = BTreeSet::new();
    let mut pending = vec![root_package.id.as_str()];
    while let Some(package_id) = pending.pop() {
        if !reachable.insert(package_id.to_string()) {
            continue;
        }
        let node = nodes.get(package_id).ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!("cargo metadata omitted dependency node {package_id}"),
            )
            .with_path(&manifest_path)
        })?;
        for dependency in &node.deps {
            pending.push(dependency.pkg.as_str());
        }
    }
    let mut local_package_roots = BTreeSet::new();
    for package in &metadata.packages {
        if package.source.is_some() || !reachable.contains(&package.id) {
            continue;
        }
        let package_root = package
            .manifest_path
            .parent()
            .unwrap_or(Path::new("."))
            .canonicalize()
            .map_err(|error| native_build_input_read_error(&package.manifest_path, error))?;
        local_package_roots.insert(package_root);
    }
    if local_package_roots.len() > NATIVE_EXTENSION_MAX_INPUT_FILES {
        return Err(native_module_input_file_limit_error(&rust_dir));
    }

    let workspace_root = metadata
        .workspace_root
        .canonicalize()
        .map_err(|error| native_build_input_read_error(&metadata.workspace_root, error))?;
    if workspace_root != located_workspace_root {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "Cargo workspace root changed between locate-project ({}) and metadata ({})",
                located_workspace_root.display(),
                workspace_root.display(),
            ),
        )
        .with_path(&manifest_path));
    }
    if metadata.target_directory != declared_target_directory {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "Cargo target directory {} does not match [extension.native].path target directory {}",
                metadata.target_directory.display(),
                declared_target_directory.display(),
            ),
        )
        .with_path(native_path));
    }
    let lock_path = located_lock_path;
    let lock_metadata = fs::symlink_metadata(&lock_path).map_err(|error| {
        let kind = if error.kind() == std::io::ErrorKind::NotFound {
            ModuleSystemErrorKind::Missing
        } else {
            ModuleSystemErrorKind::ReadFailed
        };
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            kind,
            format!(
                "native extension builds require the Cargo workspace lockfile at {}: {}",
                lock_path.display(),
                error,
            ),
        )
        .with_path(&lock_path)
    })?;
    if lock_metadata.file_type().is_symlink() || !lock_metadata.file_type().is_file() {
        return Err(native_build_input_read_error(
            &lock_path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Cargo workspace lockfile must be a regular file",
            ),
        ));
    }

    let mut metadata_hasher = StableHasher::new("vo-native-cargo-metadata-v1");
    metadata_hasher.update_bytes("metadata", &output.stdout);
    let metadata_digest = metadata_hasher.finish();
    Ok(NativeCargoContext {
        rust_dir,
        manifest_path,
        workspace_root,
        lock_path,
        target_directory: metadata.target_directory,
        root_package_id: root_package.id.clone(),
        root_package_name: root_package.name.clone(),
        root_target_name,
        local_package_roots: local_package_roots.into_iter().collect(),
        patch_configs,
        metadata_digest,
    })
}

fn locate_native_cargo_workspace_root(
    rust_dir: &Path,
    manifest_path: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<PathBuf, ModuleSystemError> {
    let mut command = std::process::Command::new("cargo");
    command
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .arg("--manifest-path")
        .arg(manifest_path)
        .current_dir(rust_dir);
    apply_workspace_discovery_to_command(&mut command, workspace_discovery);
    let output =
        run_command_with_bounded_output(&mut command, MAX_TEXT_FILE_BYTES).map_err(|error| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ReadFailed,
                format!("failed to locate native extension Cargo workspace: {error}"),
            )
            .with_path(manifest_path)
        })?;
    if output.stdout.len() > MAX_TEXT_FILE_BYTES || output.stderr.len() > MAX_TEXT_FILE_BYTES {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            "cargo locate-project output exceeded the text-file limit",
        )
        .with_path(manifest_path));
    }
    if !output.status.success() {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "cargo locate-project failed for {}: {}",
                manifest_path.display(),
                String::from_utf8_lossy(&output.stderr),
            ),
        )
        .with_path(manifest_path));
    }
    let workspace_manifest = String::from_utf8(output.stdout).map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!("cargo locate-project returned a non-UTF-8 path: {error}"),
        )
        .with_path(manifest_path)
    })?;
    let workspace_manifest = PathBuf::from(workspace_manifest.trim());
    let workspace_manifest = workspace_manifest
        .canonicalize()
        .map_err(|error| native_build_input_read_error(&workspace_manifest, error))?;
    workspace_manifest
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "Cargo workspace manifest has no parent: {}",
                    workspace_manifest.display(),
                ),
            )
            .with_path(&workspace_manifest)
        })
}

fn native_cargo_uses_volang_git_source(
    rust_dir: &Path,
    workspace_root: &Path,
    lock_bytes: &[u8],
) -> Result<bool, ModuleSystemError> {
    const SOURCE: &[u8] = b"github.com/vo-lang/volang";
    if lock_bytes
        .windows(SOURCE.len())
        .any(|window| window == SOURCE)
    {
        return Ok(true);
    }
    let mut manifests = BTreeSet::new();
    insert_existing_context_file(&mut manifests, rust_dir.join("Cargo.toml"));
    if rust_dir.starts_with(workspace_root) {
        for ancestor in rust_dir.ancestors() {
            if !ancestor.starts_with(workspace_root) {
                break;
            }
            insert_existing_context_file(&mut manifests, ancestor.join("Cargo.toml"));
            if ancestor == workspace_root {
                break;
            }
        }
    }
    for manifest in manifests {
        let bytes = read_bounded_file(&manifest, MAX_TEXT_FILE_BYTES, "Cargo manifest")
            .map_err(|error| native_build_input_read_error(&manifest, error))?;
        if bytes.windows(SOURCE.len()).any(|window| window == SOURCE) {
            return Ok(true);
        }
    }
    Ok(false)
}

fn declared_native_cargo_target_directory(
    native_path: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    let profile_dir = native_path.parent().ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "declared native extension path has no profile directory: {}",
                native_path.display(),
            ),
        )
        .with_path(native_path)
    })?;
    if profile_dir.file_name() != Some(std::ffi::OsStr::new(profile)) {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "[extension.native].path must resolve through the active Cargo profile directory '{profile}': {}",
                native_path.display(),
            ),
        )
        .with_path(native_path));
    }
    let target_dir = profile_dir.parent().ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "declared native extension path has no Cargo target directory: {}",
                native_path.display(),
            ),
        )
        .with_path(native_path)
    })?;
    normalize_native_output_path(target_dir)
}

fn normalize_native_output_path(path: &Path) -> Result<PathBuf, ModuleSystemError> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .map_err(|error| {
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::ReadFailed,
                    format!("failed to resolve the current directory: {error}"),
                )
                .with_path(path)
            })?
            .join(path)
    };
    let mut existing = absolute.as_path();
    let mut missing = Vec::<OsString>::new();
    loop {
        match existing.try_exists() {
            Ok(true) => {
                let mut normalized = existing
                    .canonicalize()
                    .map_err(|error| native_build_input_read_error(existing, error))?;
                for component in missing.iter().rev() {
                    normalized.push(component);
                }
                return Ok(normalized);
            }
            Ok(false) => {}
            Err(error) => return Err(native_build_input_read_error(existing, error)),
        }
        let name = existing.file_name().ok_or_else(|| {
            native_build_input_read_error(
                &absolute,
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "native extension output path has no existing ancestor",
                ),
            )
        })?;
        missing.push(name.to_os_string());
        existing = existing.parent().ok_or_else(|| {
            native_build_input_read_error(
                &absolute,
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "native extension output path has no existing parent",
                ),
            )
        })?;
    }
}

pub(super) fn ensure_local_native_extension_built_with_workspace(
    module_dir: &Path,
    native_path: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<PathBuf, ModuleSystemError> {
    let module_dir = module_dir
        .canonicalize()
        .map_err(|error| native_build_input_read_error(module_dir, error))?;
    let native_path = normalize_native_output_path(native_path)?;
    let rust_manifest = module_dir.join("rust").join("Cargo.toml");
    if !rust_manifest.exists() {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::Missing,
            format!(
                "native extension rust manifest is missing at {}",
                rust_manifest.display()
            ),
        )
        .with_path(&rust_manifest));
    }
    let build_lock = native_extension_build_lock(&native_path);
    let _build_guard = build_lock
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let _cross_process_build_guard =
        acquire_native_extension_build_file_lock(&module_dir, &native_path)?;
    let workspace_discovery = normalized_workspace_discovery(&module_dir, workspace_discovery)?;

    for attempt in 0..NATIVE_EXTENSION_BUILD_ATTEMPTS {
        let input_state =
            capture_native_extension_input_state(&module_dir, &native_path, &workspace_discovery)?;
        let artifact = build_native_extension(&native_path, &input_state, &workspace_discovery)?;
        if artifact.fresh
            && native_extension_cache_is_valid_for_fingerprint(
                &native_path,
                &input_state.build_token,
            )?
        {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "native_extension_cached")
                    .path(native_path.display().to_string()),
            );
        }

        let post_build_state =
            capture_native_extension_input_state(&module_dir, &native_path, &workspace_discovery)?;
        if input_state.fingerprint != post_build_state.fingerprint
            || input_state.generation != post_build_state.generation
            || input_state.build_token != post_build_state.build_token
            || input_state.cargo.metadata_digest != post_build_state.cargo.metadata_digest
        {
            emit_native_extension_inputs_changed(&module_dir);
            if attempt + 1 < NATIVE_EXTENSION_BUILD_ATTEMPTS {
                continue;
            }
            return Err(native_extension_inputs_kept_changing_error(&module_dir));
        }

        validate_built_native_extension_abi(&artifact.path, &module_dir)?;
        let load_path = materialize_native_extension_load_copy(&native_path)?;
        write_native_extension_abi_marker(&native_path)?;
        write_native_extension_abi_marker(&load_path)?;
        write_native_extension_input_marker(&native_path, &post_build_state.build_token)?;
        let final_state =
            capture_native_extension_input_state(&module_dir, &native_path, &workspace_discovery)?;
        if final_state.fingerprint == post_build_state.fingerprint
            && final_state.generation == post_build_state.generation
            && final_state.build_token == post_build_state.build_token
            && final_state.cargo.metadata_digest == post_build_state.cargo.metadata_digest
        {
            return Ok(load_path);
        }
        emit_native_extension_inputs_changed(&module_dir);
        if attempt + 1 < NATIVE_EXTENSION_BUILD_ATTEMPTS {
            continue;
        }
        return Err(native_extension_inputs_kept_changing_error(&module_dir));
    }

    unreachable!("native extension build loop always returns")
}

fn emit_native_extension_inputs_changed(module_dir: &Path) {
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "native_extension_build_inputs_changed")
            .path(module_dir.display().to_string()),
    );
}

fn native_extension_inputs_kept_changing_error(module_dir: &Path) -> ModuleSystemError {
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::BuildFailed,
        format!(
            "native extension inputs kept changing while preparing {}",
            module_dir.display(),
        ),
    )
    .with_path(module_dir)
}

#[cfg(test)]
fn native_extension_cache_is_valid(
    native_path: &Path,
    module_dir: &Path,
) -> Result<bool, ModuleSystemError> {
    let input_fingerprint = native_extension_input_fingerprint(module_dir)?;
    native_extension_cache_is_valid_for_fingerprint(native_path, &input_fingerprint)
}

fn native_extension_cache_is_valid_for_fingerprint(
    native_path: &Path,
    input_fingerprint: &str,
) -> Result<bool, ModuleSystemError> {
    if !native_path.is_file() || !native_extension_build_marker_matches(native_path) {
        return Ok(false);
    }
    let marker_path = native_extension_input_marker_path(native_path);
    let Ok(marker) = read_bounded_text_file(
        &marker_path,
        NATIVE_EXTENSION_MAX_MARKER_BYTES,
        "native extension input marker",
    ) else {
        return Ok(false);
    };
    Ok(marker == native_extension_input_marker(input_fingerprint))
}

fn native_extension_build_lock(native_path: &Path) -> Arc<Mutex<()>> {
    let key = native_path
        .parent()
        .and_then(|parent| parent.canonicalize().ok())
        .map(|parent| {
            native_path
                .file_name()
                .map_or(parent.clone(), |name| parent.join(name))
        })
        .unwrap_or_else(|| native_path.to_path_buf());
    let locks = NATIVE_BUILD_LOCKS.get_or_init(|| Mutex::new(HashMap::new()));
    let mut locks = locks
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    locks.retain(|_, lock| lock.strong_count() > 0);
    if let Some(lock) = locks.get(&key).and_then(Weak::upgrade) {
        return lock;
    }
    let lock = Arc::new(Mutex::new(()));
    locks.insert(key, Arc::downgrade(&lock));
    lock
}

fn acquire_native_extension_build_file_lock(
    module_dir: &Path,
    native_path: &Path,
) -> Result<fs::File, ModuleSystemError> {
    let lock_path = native_extension_build_file_lock_path(module_dir, native_path);
    if let Some(parent) = lock_path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::BuildFailed,
                format!(
                    "failed to create native extension build directory at {}: {}",
                    parent.display(),
                    error,
                ),
            )
            .with_path(parent)
        })?;
    }
    let file = OpenOptions::new()
        .create(true)
        .truncate(false)
        .read(true)
        .write(true)
        .open(&lock_path)
        .map_err(|error| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::BuildFailed,
                format!(
                    "failed to open native extension build lock at {}: {}",
                    lock_path.display(),
                    error,
                ),
            )
            .with_path(&lock_path)
        })?;
    file.lock().map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "failed to lock native extension build at {}: {}",
                lock_path.display(),
                error,
            ),
        )
        .with_path(&lock_path)
    })?;
    Ok(file)
}

fn native_extension_build_file_lock_path(module_dir: &Path, native_path: &Path) -> PathBuf {
    let mut hasher = StableHasher::new("vo-native-extension-build-lock-v1");
    hasher.update_path("native_path", native_path);
    module_dir
        .join("rust")
        .join("target")
        .join(format!(".vo-build-lock-{}", hasher.finish_suffix()))
}

#[cfg(test)]
fn native_extension_input_fingerprint(module_dir: &Path) -> Result<String, ModuleSystemError> {
    native_extension_input_fingerprint_with_workspace(
        module_dir,
        &vo_module::workspace::workspace_discovery_from_environment(),
    )
}

fn native_extension_input_fingerprint_with_workspace(
    module_dir: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<String, ModuleSystemError> {
    compute_native_extension_input_fingerprint(module_dir, workspace_discovery, || {
        native_build_context_fingerprint(module_dir)
    })
}

fn compute_native_extension_input_fingerprint<F>(
    module_dir: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    build_context_fingerprint: F,
) -> Result<String, ModuleSystemError>
where
    F: FnOnce() -> Result<String, ModuleSystemError>,
{
    let rust_dir = module_dir.join("rust");
    let rust_dir = rust_dir
        .canonicalize()
        .unwrap_or_else(|_| rust_dir.to_path_buf());
    let native_build_inputs = collect_native_build_inputs(&rust_dir)?;
    let module_compile_inputs =
        collect_native_module_compile_inputs(module_dir, workspace_discovery)?;
    let context_fingerprint = build_context_fingerprint()?;
    Ok(hash_native_build_inputs(
        &rust_dir,
        &native_build_inputs,
        &module_compile_inputs,
        workspace_discovery,
        &context_fingerprint,
    ))
}

fn hash_native_build_inputs(
    rust_dir: &Path,
    native_build_inputs: &[(PathBuf, Vec<u8>)],
    module_compile_inputs: &[(PathBuf, Vec<u8>)],
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    context_fingerprint: &str,
) -> String {
    let mut sorted_native_inputs = native_build_inputs.iter().collect::<Vec<_>>();
    sorted_native_inputs.sort_by(|left, right| left.0.cmp(&right.0));
    let mut sorted_module_inputs = module_compile_inputs.iter().collect::<Vec<_>>();
    sorted_module_inputs.sort_by(|left, right| left.0.cmp(&right.0));

    let mut hasher = StableHasher::new("vo-native-extension-inputs-v2");
    hasher.update_path("rust_dir", rust_dir);
    hasher.update_str("build_context", context_fingerprint);
    match workspace_discovery {
        vo_module::workspace::WorkspaceDiscovery::Auto => {
            hasher.update_str("workspace_discovery", "auto");
        }
        vo_module::workspace::WorkspaceDiscovery::Disabled => {
            hasher.update_str("workspace_discovery", "disabled");
        }
        vo_module::workspace::WorkspaceDiscovery::Explicit(path) => {
            hasher.update_str("workspace_discovery", "explicit");
            hasher.update_path("workspace_file", path);
        }
    }
    for (path, bytes) in sorted_native_inputs {
        hasher.update_path("native_input_path", path);
        hasher.update_bytes("native_input_bytes", bytes);
    }
    for (path, bytes) in sorted_module_inputs {
        hasher.update_path("module_input_path", path);
        hasher.update_bytes("module_input_bytes", bytes);
    }
    hasher.finish()
}

#[derive(Debug, Clone)]
struct NativeExtensionInputState {
    fingerprint: String,
    generation: String,
    build_token: String,
    cargo: NativeCargoContext,
}

fn capture_native_extension_input_state(
    module_dir: &Path,
    native_path: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<NativeExtensionInputState, ModuleSystemError> {
    let mut previous =
        capture_native_extension_input_state_once(module_dir, native_path, workspace_discovery)?;
    for _ in 0..NATIVE_EXTENSION_BUILD_ATTEMPTS {
        let current = capture_native_extension_input_state_once(
            module_dir,
            native_path,
            workspace_discovery,
        )?;
        if current.fingerprint == previous.fingerprint
            && current.generation == previous.generation
            && current.build_token == previous.build_token
            && current.cargo.metadata_digest == previous.cargo.metadata_digest
        {
            return Ok(current);
        }
        previous = current;
    }
    Err(native_extension_inputs_kept_changing_error(module_dir))
}

fn capture_native_extension_input_state_once(
    module_dir: &Path,
    native_path: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<NativeExtensionInputState, ModuleSystemError> {
    let cargo = inspect_native_cargo_context(module_dir, native_path, workspace_discovery)?;
    let base_fingerprint =
        native_extension_input_fingerprint_with_workspace(module_dir, workspace_discovery)?;
    let cargo_lock_bytes = read_bounded_file(
        &cargo.lock_path,
        MAX_TEXT_FILE_BYTES,
        "Cargo workspace lockfile",
    )
    .map_err(|error| native_build_input_read_error(&cargo.lock_path, error))?;
    let cargo_inputs = collect_cargo_local_package_inputs(&cargo.local_package_roots)?;
    let (module_roots, workfile) = native_module_input_roots(module_dir, workspace_discovery)?;

    let mut hasher = StableHasher::new("vo-native-extension-production-inputs-v1");
    hasher.update_str("base_fingerprint", &base_fingerprint);
    hasher.update_str("cargo_metadata", &cargo.metadata_digest);
    hasher.update_path("cargo_workspace_root", &cargo.workspace_root);
    hasher.update_path("cargo_lock", &cargo.lock_path);
    hasher.update_bytes("cargo_lock_bytes", &cargo_lock_bytes);
    hasher.update_path("cargo_target_directory", &cargo.target_directory);
    hasher.update_str("cargo_root_package", &cargo.root_package_id);
    hasher.update_str("cargo_root_target", &cargo.root_target_name);
    for config in &cargo.patch_configs {
        hasher.update_str("cargo_patch_config", config);
    }
    for (path, bytes) in &cargo_inputs.files {
        hasher.update_path("cargo_local_input_path", path);
        hasher.update_bytes("cargo_local_input_bytes", bytes);
    }
    let fingerprint = hasher.finish();

    let mut generation_roots = module_roots.into_iter().collect::<Vec<_>>();
    generation_roots.extend(cargo.local_package_roots.iter().cloned());
    let mut generation_files = cargo_inputs
        .files
        .iter()
        .map(|(path, _)| path.clone())
        .collect::<Vec<_>>();
    generation_files.extend(native_build_context_files(&cargo.rust_dir)?);
    generation_files.push(cargo.manifest_path.clone());
    generation_files.push(cargo.lock_path.clone());
    if let Some(workfile) = workfile {
        generation_files.push(workfile);
    }
    let generation =
        native_input_generation(generation_roots, generation_files, cargo_inputs.directories)?;
    let mut token_hasher = StableHasher::new("vo-native-extension-build-token-v1");
    token_hasher.update_str("fingerprint", &fingerprint);
    token_hasher.update_str("generation", &generation);
    let build_token = token_hasher.finish();
    Ok(NativeExtensionInputState {
        fingerprint,
        generation,
        build_token,
        cargo,
    })
}

fn minimal_non_overlapping_roots(mut roots: Vec<PathBuf>) -> Vec<PathBuf> {
    roots.sort_by(|left, right| {
        left.components()
            .count()
            .cmp(&right.components().count())
            .then_with(|| left.cmp(right))
    });
    let mut minimal = Vec::<PathBuf>::new();
    for root in roots {
        if minimal.iter().any(|parent| root.starts_with(parent)) {
            continue;
        }
        minimal.push(root);
    }
    minimal
}

struct NativeCargoInputs {
    files: Vec<(PathBuf, Vec<u8>)>,
    directories: BTreeSet<PathBuf>,
}

fn collect_cargo_local_package_inputs(
    roots: &[PathBuf],
) -> Result<NativeCargoInputs, ModuleSystemError> {
    let roots = minimal_non_overlapping_roots(roots.to_vec());
    let mut paths = Vec::new();
    let mut walk = NativeInputWalkState::default();
    for root in roots {
        collect_native_build_input_paths(&root, &root, &mut walk, 0, &mut paths)?;
    }
    paths.sort();
    paths.dedup();
    let mut total_bytes = 0usize;
    let mut inputs = Vec::with_capacity(paths.len());
    for path in paths {
        let canonical = path
            .canonicalize()
            .map_err(|error| native_build_input_read_error(&path, error))?;
        let bytes = read_bounded_file(
            &canonical,
            NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES,
            "native Cargo local package input",
        )
        .map_err(|error| native_build_input_read_error(&canonical, error))?;
        total_bytes = total_bytes.checked_add(bytes.len()).ok_or_else(|| {
            native_build_input_read_error(
                &canonical,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native Cargo local package input size overflow",
                ),
            )
        })?;
        if total_bytes > NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES {
            return Err(native_build_input_read_error(
                &canonical,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native Cargo local package inputs exceed the {}-byte limit",
                        NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES,
                    ),
                ),
            ));
        }
        inputs.push((canonical, bytes));
    }
    Ok(NativeCargoInputs {
        files: inputs,
        directories: walk.visited_dirs,
    })
}

fn native_input_generation(
    roots: Vec<PathBuf>,
    files: Vec<PathBuf>,
    known_dirs: BTreeSet<PathBuf>,
) -> Result<String, ModuleSystemError> {
    let roots = minimal_non_overlapping_roots(
        roots
            .into_iter()
            .map(|root| root.canonicalize().unwrap_or(root))
            .collect(),
    );
    let mut generation_paths = BTreeSet::new();
    generation_paths.extend(known_dirs);
    let mut walk = NativeInputWalkState::default();
    let mut walked_files = Vec::new();
    for root in roots {
        collect_native_build_input_paths(&root, &root, &mut walk, 0, &mut walked_files)?;
    }
    generation_paths.extend(walk.visited_dirs);
    generation_paths.extend(walked_files);
    generation_paths.extend(files.into_iter().filter(|path| path.exists()));
    if generation_paths.len() > NATIVE_EXTENSION_MAX_INPUT_ENTRIES {
        return Err(native_module_input_entry_limit_error(Path::new(
            "native Cargo inputs",
        )));
    }

    let mut hasher = StableHasher::new("vo-native-extension-input-generation-v1");
    for path in generation_paths {
        let metadata = fs::symlink_metadata(&path)
            .map_err(|error| native_build_input_read_error(&path, error))?;
        if metadata.file_type().is_symlink()
            || !(metadata.file_type().is_file() || metadata.file_type().is_dir())
        {
            return Err(native_build_input_read_error(
                &path,
                unsupported_native_input_type_error(&path),
            ));
        }
        hasher.update_path("path", &path);
        update_generation_metadata(&mut hasher, &metadata);
    }
    Ok(hasher.finish())
}

#[cfg(unix)]
fn update_generation_metadata(hasher: &mut StableHasher, metadata: &fs::Metadata) {
    use std::os::unix::fs::MetadataExt;

    for (label, bytes) in [
        ("dev", metadata.dev().to_le_bytes().to_vec()),
        ("ino", metadata.ino().to_le_bytes().to_vec()),
        ("mode", metadata.mode().to_le_bytes().to_vec()),
        ("len", metadata.len().to_le_bytes().to_vec()),
        ("mtime", metadata.mtime().to_le_bytes().to_vec()),
        ("mtime_nsec", metadata.mtime_nsec().to_le_bytes().to_vec()),
        ("ctime", metadata.ctime().to_le_bytes().to_vec()),
        ("ctime_nsec", metadata.ctime_nsec().to_le_bytes().to_vec()),
    ] {
        hasher.update_bytes(label, &bytes);
    }
}

#[cfg(windows)]
fn update_generation_metadata(hasher: &mut StableHasher, metadata: &fs::Metadata) {
    use std::os::windows::fs::MetadataExt;

    for (label, bytes) in [
        (
            "attributes",
            metadata.file_attributes().to_le_bytes().to_vec(),
        ),
        ("len", metadata.file_size().to_le_bytes().to_vec()),
        ("created", metadata.creation_time().to_le_bytes().to_vec()),
        ("written", metadata.last_write_time().to_le_bytes().to_vec()),
        (
            "volume",
            metadata
                .volume_serial_number()
                .unwrap_or_default()
                .to_le_bytes()
                .to_vec(),
        ),
        (
            "index",
            metadata
                .file_index()
                .unwrap_or_default()
                .to_le_bytes()
                .to_vec(),
        ),
    ] {
        hasher.update_bytes(label, &bytes);
    }
}

#[cfg(not(any(unix, windows)))]
fn update_generation_metadata(hasher: &mut StableHasher, metadata: &fs::Metadata) {
    hasher.update_bytes("len", &metadata.len().to_le_bytes());
    hasher.update_str("modified", &format!("{:?}", metadata.modified()));
    hasher.update_str("created", &format!("{:?}", metadata.created()));
    hasher.update_bool("file", metadata.file_type().is_file());
    hasher.update_bool("dir", metadata.file_type().is_dir());
}

fn collect_native_module_compile_inputs(
    module_dir: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<Vec<(PathBuf, Vec<u8>)>, ModuleSystemError> {
    let (roots, workfile) = native_module_input_roots(module_dir, workspace_discovery)?;
    collect_native_module_compile_inputs_from_roots(module_dir, roots, workfile)
}

fn native_module_input_roots(
    module_dir: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<(BTreeSet<PathBuf>, Option<PathBuf>), ModuleSystemError> {
    let module_dir = module_dir
        .canonicalize()
        .unwrap_or_else(|_| module_dir.to_path_buf());
    let mod_path = module_dir.join("vo.mod");
    let mod_content = read_bounded_text_file(
        &mod_path,
        MAX_TEXT_FILE_BYTES,
        "native extension module manifest",
    )
    .map_err(|error| native_build_input_read_error(&mod_path, error))?;
    let mod_file = ModFile::parse(&mod_content).map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ParseFailed,
            format!(
                "failed to parse native extension module manifest at {}: {}",
                mod_path.display(),
                error,
            ),
        )
        .with_path(&mod_path)
    })?;
    let vfs = RealFs::new(".");
    let (workfile, overrides) = vo_module::workspace::load_workspace_overrides_in_with_provenance(
        &vfs,
        &module_dir,
        Some(&mod_file.module),
        workspace_discovery,
    )
    .map_err(|error| native_workspace_input_error(&module_dir, error))?;

    let mut roots = BTreeSet::new();
    roots.insert(module_dir.clone());
    for workspace_override in overrides {
        roots.insert(
            workspace_override
                .local_dir
                .canonicalize()
                .unwrap_or(workspace_override.local_dir),
        );
    }
    Ok((roots, workfile))
}

fn collect_native_module_compile_inputs_from_roots(
    module_dir: &Path,
    roots: BTreeSet<PathBuf>,
    workfile: Option<PathBuf>,
) -> Result<Vec<(PathBuf, Vec<u8>)>, ModuleSystemError> {
    if roots.len() > NATIVE_EXTENSION_MAX_INPUT_ENTRIES {
        return Err(native_module_input_entry_limit_error(module_dir));
    }
    let mut paths = BTreeSet::new();
    let mut scanned_entries = roots.len();
    for root in roots {
        let remaining_entries = NATIVE_EXTENSION_MAX_INPUT_ENTRIES.saturating_sub(scanned_entries);
        let (relative_paths, root_entries) =
            collect_module_compile_input_files(&root, remaining_entries)
                .map_err(|error| native_module_compile_input_error(&root, error))?;
        scanned_entries = scanned_entries
            .checked_add(root_entries)
            .ok_or_else(|| native_module_input_entry_limit_error(module_dir))?;
        if scanned_entries > NATIVE_EXTENSION_MAX_INPUT_ENTRIES {
            return Err(native_module_input_entry_limit_error(module_dir));
        }
        for relative_path in relative_paths {
            let path = root.join(relative_path);
            let canonical = path
                .canonicalize()
                .map_err(|error| native_build_input_read_error(&path, error))?;
            if !canonical.starts_with(&root) {
                return Err(native_build_input_read_error(
                    &path,
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!(
                            "native extension module input resolves outside {}",
                            root.display(),
                        ),
                    ),
                ));
            }
            if paths.insert(canonical) && paths.len() > NATIVE_EXTENSION_MAX_INPUT_FILES {
                return Err(native_module_input_file_limit_error(module_dir));
            }
        }
    }
    if let Some(workfile) = workfile {
        let metadata = fs::symlink_metadata(&workfile)
            .map_err(|error| native_build_input_read_error(&workfile, error))?;
        if !metadata.file_type().is_file() {
            return Err(native_build_input_read_error(
                &workfile,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "active vo.work is not a regular file",
                ),
            ));
        }
        let canonical = workfile
            .canonicalize()
            .map_err(|error| native_build_input_read_error(&workfile, error))?;
        if paths.insert(canonical) && paths.len() > NATIVE_EXTENSION_MAX_INPUT_FILES {
            return Err(native_module_input_file_limit_error(module_dir));
        }
    }

    let mut total_bytes = 0usize;
    let mut inputs = Vec::with_capacity(paths.len());
    for path in paths {
        let bytes = read_bounded_file(
            &path,
            NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES,
            "native extension module input",
        )
        .map_err(|error| native_build_input_read_error(&path, error))?;
        total_bytes = total_bytes.checked_add(bytes.len()).ok_or_else(|| {
            native_build_input_read_error(
                module_dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension module input size overflow",
                ),
            )
        })?;
        if total_bytes > NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES {
            return Err(native_build_input_read_error(
                module_dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native extension module inputs exceed the {}-byte limit",
                        NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES,
                    ),
                ),
            ));
        }
        inputs.push((path, bytes));
    }
    Ok(inputs)
}

fn native_module_input_file_limit_error(module_dir: &Path) -> ModuleSystemError {
    native_build_input_read_error(
        module_dir,
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "native extension module inputs exceed the {}-file limit",
                NATIVE_EXTENSION_MAX_INPUT_FILES,
            ),
        ),
    )
}

fn native_module_input_entry_limit_error(module_dir: &Path) -> ModuleSystemError {
    native_build_input_read_error(
        module_dir,
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "native extension module input trees exceed the {}-entry limit",
                NATIVE_EXTENSION_MAX_INPUT_ENTRIES,
            ),
        ),
    )
}

fn native_workspace_input_error(module_dir: &Path, error: vo_module::Error) -> ModuleSystemError {
    let kind = if matches!(&error, vo_module::Error::Io(_)) {
        ModuleSystemErrorKind::ReadFailed
    } else {
        ModuleSystemErrorKind::ValidationFailed
    };
    ModuleSystemError::new(
        ModuleSystemStage::Workspace,
        kind,
        format!(
            "failed to capture native extension workspace inputs for {}: {}",
            module_dir.display(),
            error,
        ),
    )
    .with_path(module_dir)
}

fn native_module_compile_input_error(root: &Path, error: super::CompileError) -> ModuleSystemError {
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::ReadFailed,
        format!(
            "failed to capture native extension module inputs at {}: {}",
            root.display(),
            error,
        ),
    )
    .with_path(root)
}

fn collect_native_build_inputs(
    rust_dir: &Path,
) -> Result<Vec<(PathBuf, Vec<u8>)>, ModuleSystemError> {
    let mut paths = Vec::new();
    let mut walk = NativeInputWalkState::default();
    collect_native_build_input_paths(rust_dir, rust_dir, &mut walk, 0, &mut paths)?;
    paths.sort();
    let mut total_bytes = 0usize;
    let mut inputs = Vec::with_capacity(paths.len());
    for path in paths {
        let relative = path.strip_prefix(rust_dir).unwrap_or(&path).to_path_buf();
        let bytes = read_bounded_file(
            &path,
            NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES,
            "native build input",
        )
        .map_err(|error| native_build_input_read_error(&path, error))?;
        total_bytes = total_bytes.checked_add(bytes.len()).ok_or_else(|| {
            native_build_input_read_error(
                rust_dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension input tree size overflow",
                ),
            )
        })?;
        if total_bytes > NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES {
            return Err(native_build_input_read_error(
                rust_dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native extension input tree exceeds the {}-byte limit",
                        NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES,
                    ),
                ),
            ));
        }
        inputs.push((relative, bytes));
    }
    Ok(inputs)
}

struct NativeInputWalkState {
    visited_dirs: BTreeSet<PathBuf>,
    entries: usize,
    entry_limit: usize,
}

impl NativeInputWalkState {
    fn with_entry_limit(entry_limit: usize) -> Self {
        Self {
            visited_dirs: BTreeSet::new(),
            entries: 0,
            entry_limit,
        }
    }
}

impl Default for NativeInputWalkState {
    fn default() -> Self {
        Self::with_entry_limit(NATIVE_EXTENSION_MAX_INPUT_ENTRIES)
    }
}

fn collect_native_build_input_paths(
    rust_dir: &Path,
    dir: &Path,
    walk: &mut NativeInputWalkState,
    depth: usize,
    out: &mut Vec<PathBuf>,
) -> Result<(), ModuleSystemError> {
    let mut pending = vec![(dir.to_path_buf(), depth)];
    while let Some((dir, depth)) = pending.pop() {
        if depth > NATIVE_EXTENSION_MAX_INPUT_DIRECTORY_DEPTH {
            return Err(native_build_input_read_error(
                &dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native extension input tree exceeds the maximum directory depth of {}",
                        NATIVE_EXTENSION_MAX_INPUT_DIRECTORY_DEPTH,
                    ),
                ),
            ));
        }
        let metadata = fs::symlink_metadata(&dir)
            .map_err(|error| native_build_input_read_error(&dir, error))?;
        if metadata.file_type().is_symlink() || !metadata.file_type().is_dir() {
            return Err(native_build_input_read_error(
                &dir,
                unsupported_native_input_type_error(&dir),
            ));
        }
        let canonical_dir = dir
            .canonicalize()
            .map_err(|error| native_build_input_read_error(&dir, error))?;
        if !walk.visited_dirs.insert(canonical_dir) {
            return Err(native_build_input_read_error(
                &dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native extension input directory alias or cycle at {}",
                        dir.display(),
                    ),
                ),
            ));
        }
        let remaining_entries = walk.entry_limit.saturating_sub(walk.entries);
        let mut entries = fs::read_dir(&dir)
            .map_err(|error| native_build_input_read_error(&dir, error))?
            .take(remaining_entries.saturating_add(1))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| native_build_input_read_error(&dir, error))?;
        walk.entries = walk.entries.checked_add(entries.len()).ok_or_else(|| {
            native_build_input_read_error(
                rust_dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension input entry count overflow",
                ),
            )
        })?;
        if walk.entries > walk.entry_limit {
            return Err(native_build_input_read_error(
                &dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native extension input tree exceeds the {}-entry limit",
                        walk.entry_limit,
                    ),
                ),
            ));
        }
        entries.sort_by_key(|entry| entry.file_name());
        let mut child_dirs = Vec::new();
        for entry in entries {
            let path = entry.path();
            let file_type = entry
                .file_type()
                .map_err(|error| native_build_input_read_error(&path, error))?;
            if file_type.is_symlink() {
                return Err(native_build_input_read_error(
                    &path,
                    unsupported_native_input_type_error(&path),
                ));
            }
            if file_type.is_dir() {
                if !should_skip_native_build_input_dir(&path) {
                    child_dirs.push(path);
                }
            } else if file_type.is_file() {
                if should_skip_native_build_output_file(&path) {
                    continue;
                }
                if out.len() >= NATIVE_EXTENSION_MAX_INPUT_FILES {
                    return Err(native_build_input_read_error(
                        rust_dir,
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!(
                                "native extension input tree exceeds the {}-file limit",
                                NATIVE_EXTENSION_MAX_INPUT_FILES,
                            ),
                        ),
                    ));
                }
                out.push(path);
            } else {
                return Err(native_build_input_read_error(
                    &path,
                    unsupported_native_input_type_error(&path),
                ));
            }
        }
        for child in child_dirs.into_iter().rev() {
            pending.push((child, depth.saturating_add(1)));
        }
        debug_assert!(dir == rust_dir || dir.starts_with(rust_dir));
    }
    Ok(())
}

fn unsupported_native_input_type_error(path: &Path) -> std::io::Error {
    std::io::Error::new(
        std::io::ErrorKind::InvalidData,
        format!(
            "native extension input at {} must be a regular file or directory; symbolic links and special files are unsupported",
            path.display(),
        ),
    )
}

fn should_skip_native_build_input_dir(path: &Path) -> bool {
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some("target") | Some(".git")
    )
}

fn should_skip_native_build_output_file(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| {
            name.contains(".voabi-")
                || name.ends_with(".vo-abi")
                || name.ends_with(".vo-inputs")
                || name.starts_with(".vo-build-lock-")
        })
}

fn native_build_input_read_error(path: &Path, error: std::io::Error) -> ModuleSystemError {
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::ReadFailed,
        format!(
            "failed to read native extension build input at {}: {}",
            path.display(),
            error,
        ),
    )
    .with_path(path)
}

fn read_bounded_file(path: &Path, max_bytes: usize, kind: &str) -> std::io::Result<Vec<u8>> {
    let file = fs::File::open(path)?;
    let max_bytes_u64 = u64::try_from(max_bytes).unwrap_or(u64::MAX);
    if file.metadata()?.len() > max_bytes_u64 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "{kind} at {} exceeds the {}-byte limit",
                path.display(),
                max_bytes,
            ),
        ));
    }
    let mut bytes = Vec::new();
    file.take(max_bytes_u64.saturating_add(1))
        .read_to_end(&mut bytes)?;
    if bytes.len() > max_bytes {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "{kind} at {} exceeds the {}-byte limit",
                path.display(),
                max_bytes,
            ),
        ));
    }
    Ok(bytes)
}

fn read_bounded_text_file(path: &Path, max_bytes: usize, kind: &str) -> std::io::Result<String> {
    let bytes = read_bounded_file(path, max_bytes, kind)?;
    String::from_utf8(bytes)
        .map_err(|error| std::io::Error::new(std::io::ErrorKind::InvalidData, error.utf8_error()))
}

#[derive(Debug)]
struct BoundedCommandOutput {
    status: std::process::ExitStatus,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

fn run_command_with_bounded_output(
    command: &mut std::process::Command,
    max_bytes_per_stream: usize,
) -> std::io::Result<BoundedCommandOutput> {
    command
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    let mut child = command.spawn()?;
    let stdout = child
        .stdout
        .take()
        .expect("piped command stdout must be available");
    let stderr = child
        .stderr
        .take()
        .expect("piped command stderr must be available");
    let stdout_reader = std::thread::spawn(move || {
        read_bounded_command_pipe(stdout, max_bytes_per_stream, "stdout")
    });
    let stderr_reader = std::thread::spawn(move || {
        read_bounded_command_pipe(stderr, max_bytes_per_stream, "stderr")
    });
    let status = child.wait();
    let stdout = join_command_output_reader(stdout_reader);
    let stderr = join_command_output_reader(stderr_reader);
    Ok(BoundedCommandOutput {
        status: status?,
        stdout: stdout?,
        stderr: stderr?,
    })
}

fn read_bounded_command_pipe<R: Read>(
    reader: R,
    max_bytes: usize,
    stream: &str,
) -> std::io::Result<Vec<u8>> {
    let read_limit = u64::try_from(max_bytes)
        .unwrap_or(u64::MAX)
        .saturating_add(1);
    let mut bytes = Vec::new();
    reader.take(read_limit).read_to_end(&mut bytes)?;
    if bytes.len() > max_bytes {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("command {stream} exceeds the {max_bytes}-byte limit"),
        ));
    }
    Ok(bytes)
}

fn join_command_output_reader(
    reader: std::thread::JoinHandle<std::io::Result<Vec<u8>>>,
) -> std::io::Result<Vec<u8>> {
    reader
        .join()
        .map_err(|_| std::io::Error::other("bounded command output reader panicked"))?
}

pub(super) fn native_build_context_fingerprint(
    module_dir: &Path,
) -> Result<String, ModuleSystemError> {
    let rust_dir = module_dir.join("rust");
    let rust_dir = rust_dir
        .canonicalize()
        .unwrap_or_else(|_| rust_dir.to_path_buf());
    let mut hasher = StableHasher::new("vo-native-extension-build-context-v1");
    hasher.update_path("rust_dir", &rust_dir);
    hasher.update_str("target", current_target_triple());
    hasher.update_str(
        "profile",
        if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        },
    );
    hasher.update_str("features", "default");
    hasher.update_str("compiler_build_id", env!("VO_COMPILER_BUILD_ID"));
    hasher.update_str("tool_identity", &native_tool_identity(&rust_dir)?);

    let mut context_bytes = 0usize;
    for path in native_build_context_files(&rust_dir)? {
        let bytes = read_bounded_file(&path, MAX_TEXT_FILE_BYTES, "native build context")
            .map_err(|error| native_build_input_read_error(&path, error))?;
        context_bytes = context_bytes.checked_add(bytes.len()).ok_or_else(|| {
            native_build_input_read_error(
                &rust_dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native build context size overflow",
                ),
            )
        })?;
        if context_bytes > NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES {
            return Err(native_build_input_read_error(
                &rust_dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native build context exceeds the {}-byte limit",
                        NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES,
                    ),
                ),
            ));
        }
        hasher.update_path("context_path", &path);
        hasher.update_bytes("context_bytes", &bytes);
    }

    let mut environment = std::env::vars_os()
        .filter(|(key, _)| {
            let key = key.as_os_str();
            key != std::ffi::OsStr::new(NATIVE_EXTENSION_FFI_SOURCE_FINGERPRINT_ENV)
                && key != std::ffi::OsStr::new("VOWORK")
                && key != std::ffi::OsStr::new("CARGO_TARGET_DIR")
                && key != std::ffi::OsStr::new("CARGO_BUILD_TARGET")
                && !key.to_str().is_some_and(|key| key.starts_with("VO_JIT_"))
        })
        .collect::<Vec<(OsString, OsString)>>();
    environment.sort();
    for (key, value) in environment {
        hasher.update_path("environment_key", Path::new(&key));
        hasher.update_path("environment_value", Path::new(&value));
    }

    Ok(hasher.finish())
}

fn native_build_context_files(rust_dir: &Path) -> Result<Vec<PathBuf>, ModuleSystemError> {
    let mut files = BTreeSet::new();
    if let Some(parent) = rust_dir.parent() {
        for ancestor in parent.ancestors() {
            for relative in [
                "Cargo.toml",
                "Cargo.lock",
                "rust-toolchain.toml",
                "rust-toolchain",
                ".cargo/config.toml",
                ".cargo/config",
            ] {
                insert_existing_context_file(&mut files, ancestor.join(relative));
            }
        }
    }

    if let Some(cargo_home) = cargo_home_dir() {
        insert_existing_context_file(&mut files, cargo_home.join("config.toml"));
        insert_existing_context_file(&mut files, cargo_home.join("config"));
    }

    Ok(files.into_iter().collect())
}

#[cfg(test)]
fn collect_local_patch_input_files(
    dir: &Path,
    files: &mut BTreeSet<PathBuf>,
    walk: &mut NativeInputWalkState,
) -> std::io::Result<()> {
    let mut pending = vec![(dir.to_path_buf(), 0usize)];
    while let Some((dir, depth)) = pending.pop() {
        if depth > NATIVE_EXTENSION_MAX_INPUT_DIRECTORY_DEPTH {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "local Volang patch inputs exceed the maximum directory depth of {} at {}",
                    NATIVE_EXTENSION_MAX_INPUT_DIRECTORY_DEPTH,
                    dir.display(),
                ),
            ));
        }
        let metadata = fs::symlink_metadata(&dir)?;
        if metadata.file_type().is_symlink() || !metadata.file_type().is_dir() {
            return Err(unsupported_native_input_type_error(&dir));
        }
        let canonical_dir = dir.canonicalize()?;
        if !walk.visited_dirs.insert(canonical_dir) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "local Volang patch input directory alias or cycle at {}",
                    dir.display(),
                ),
            ));
        }
        let remaining_entries = walk.entry_limit.saturating_sub(walk.entries);
        let mut entries = fs::read_dir(&dir)?
            .take(remaining_entries.saturating_add(1))
            .collect::<Result<Vec<_>, _>>()?;
        walk.entries = walk.entries.checked_add(entries.len()).ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "local Volang patch input entry count overflow",
            )
        })?;
        if walk.entries > walk.entry_limit {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "local Volang patch input trees at {} exceed the {}-entry limit",
                    dir.display(),
                    walk.entry_limit,
                ),
            ));
        }
        entries.sort_by_key(|entry| entry.file_name());
        let mut child_dirs = Vec::new();
        for entry in entries {
            let path = entry.path();
            let file_type = entry.file_type()?;
            if file_type.is_symlink() {
                return Err(unsupported_native_input_type_error(&path));
            }
            if file_type.is_dir() {
                if !matches!(
                    path.file_name().and_then(|name| name.to_str()),
                    Some("target")
                        | Some(".git")
                        | Some("pkg")
                        | Some("pkg-island")
                        | Some("node_modules")
                ) {
                    child_dirs.push(path);
                }
            } else if file_type.is_file() {
                let path = path.canonicalize().unwrap_or(path);
                if !files.contains(&path) && files.len() >= NATIVE_EXTENSION_MAX_INPUT_FILES {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!(
                            "local Volang patch inputs exceed the {}-file limit",
                            NATIVE_EXTENSION_MAX_INPUT_FILES,
                        ),
                    ));
                }
                files.insert(path);
            } else {
                return Err(unsupported_native_input_type_error(&path));
            }
        }
        for child in child_dirs.into_iter().rev() {
            pending.push((child, depth.saturating_add(1)));
        }
    }
    Ok(())
}

fn insert_existing_context_file(files: &mut BTreeSet<PathBuf>, path: PathBuf) {
    if path.is_file() {
        files.insert(path.canonicalize().unwrap_or(path));
    }
}

fn cargo_home_dir() -> Option<PathBuf> {
    std::env::var_os("CARGO_HOME")
        .map(PathBuf::from)
        .or_else(|| {
            std::env::var_os("HOME")
                .or_else(|| std::env::var_os("USERPROFILE"))
                .map(|home| PathBuf::from(home).join(".cargo"))
        })
}

fn native_tool_identity(rust_dir: &Path) -> Result<String, ModuleSystemError> {
    let mut hasher = StableHasher::new("vo-native-extension-tools-v1");
    for tool in ["cargo", "rustc"] {
        let mut command = std::process::Command::new(tool);
        command
            .args(["--version", "--verbose"])
            .current_dir(rust_dir);
        let output = run_command_with_bounded_output(&mut command, MAX_TEXT_FILE_BYTES).map_err(
            |error| {
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::ReadFailed,
                    format!("failed to inspect {tool}: {error}"),
                )
                .with_path(rust_dir)
            },
        )?;
        if !output.status.success() {
            return Err(ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ReadFailed,
                format!(
                    "failed to inspect {tool}: {}",
                    String::from_utf8_lossy(&output.stderr),
                ),
            )
            .with_path(rust_dir));
        }
        hasher.update_str("tool", tool);
        hasher.update_bytes("stdout", &output.stdout);
        hasher.update_bytes("stderr", &output.stderr);
    }
    Ok(hasher.finish())
}

fn materialize_native_extension_load_copy(
    native_path: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    let bytes = read_bounded_file(
        native_path,
        NATIVE_EXTENSION_MAX_ARTIFACT_BYTES,
        "native extension artifact",
    )
    .map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ReadFailed,
            format!(
                "failed to read native extension at {}: {}",
                native_path.display(),
                e
            ),
        )
        .with_path(native_path)
    })?;
    let load_path = native_extension_load_copy_path_for_bytes(native_path, &bytes);
    let expected_load_marker = native_extension_abi_marker_for_bytes(&bytes);
    let load_copy_matches = native_extension_build_marker_matches(&load_path)
        && read_bounded_text_file(
            &native_extension_abi_marker_path(&load_path),
            NATIVE_EXTENSION_MAX_MARKER_BYTES,
            "native extension ABI marker",
        )
        .is_ok_and(|marker| marker == expected_load_marker);
    if !load_copy_matches {
        atomic_write_native_file(&load_path, &bytes).map_err(|e| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::BuildFailed,
                format!(
                    "failed to write native extension load copy at {}: {}",
                    load_path.display(),
                    e,
                ),
            )
            .with_path(&load_path)
        })?;
    }
    write_native_extension_abi_marker(&load_path)?;
    Ok(load_path)
}

fn native_extension_load_copy_path_for_bytes(native_path: &Path, bytes: &[u8]) -> PathBuf {
    let mut hasher = StableHasher::new("vo-native-extension-load-copy-v1");
    hasher.update_str("abi", &current_native_extension_abi_identity());
    hasher.update_bytes("dylib", bytes);
    let digest = hasher.finish_suffix();
    native_extension_load_copy_path(native_path, &digest)
}

fn native_extension_load_copy_path(native_path: &Path, copy_id: &str) -> PathBuf {
    let stem = native_path
        .file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("native-extension");
    let abi = format!("{}-{ABI_FINGERPRINT:016x}", ABI_VERSION);
    let file_name = match native_path.extension().and_then(|ext| ext.to_str()) {
        Some(ext) => format!("{stem}.voabi-{abi}-{copy_id}.{ext}"),
        None => format!("{stem}.voabi-{abi}-{copy_id}"),
    };
    native_path.with_file_name(file_name)
}

pub(super) fn native_extension_build_marker_matches(native_path: &Path) -> bool {
    let marker_path = native_extension_abi_marker_path(native_path);
    let Ok(marker) = read_bounded_text_file(
        &marker_path,
        NATIVE_EXTENSION_MAX_MARKER_BYTES,
        "native extension ABI marker",
    ) else {
        return false;
    };
    let Ok(expected) = native_extension_abi_marker(native_path) else {
        return false;
    };
    marker == expected
}

pub(super) fn cached_native_extension_spec_is_current_with_workspace(
    spec: &NativeExtensionSpec,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> bool {
    if !is_local_native_extension_load_copy(&spec.native_path) {
        return spec.native_path.is_file();
    }
    if !native_extension_build_marker_matches(&spec.native_path) {
        return false;
    }

    let Ok(content) = read_bounded_text_file(
        &spec.manifest_path,
        MAX_TEXT_FILE_BYTES,
        "native extension manifest",
    ) else {
        return false;
    };
    let Ok(mod_file) = ModFile::parse(&content) else {
        return false;
    };
    let Some(module_owner) = mod_file.module.as_github() else {
        return false;
    };
    if module_owner.as_str() != spec.module_owner {
        return false;
    }
    let Ok(manifest) = parse_ext_manifest_content(&content, &spec.manifest_path) else {
        return false;
    };
    if manifest.name != spec.name {
        return false;
    }
    let Some(module_dir) = spec.manifest_path.parent() else {
        return false;
    };
    let module_dir = module_dir
        .canonicalize()
        .unwrap_or_else(|_| module_dir.to_path_buf());
    let Ok(native_path) = manifest_local_native_path(&manifest, &module_dir) else {
        return false;
    };
    let Ok(current_load_path) = ensure_local_native_extension_built_with_workspace(
        &module_dir,
        &native_path,
        workspace_discovery,
    ) else {
        return false;
    };
    paths_refer_to_same_file(&current_load_path, &spec.native_path)
}

fn is_local_native_extension_load_copy(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.contains(".voabi-"))
}

fn paths_refer_to_same_file(left: &Path, right: &Path) -> bool {
    if left == right {
        return true;
    }
    match (left.canonicalize(), right.canonicalize()) {
        (Ok(left), Ok(right)) => left == right,
        _ => false,
    }
}

fn native_extension_host_abi_error(native_path: &Path, module_owner: &str) -> Option<String> {
    let mut loader = ExtensionLoader::new();
    loader
        .load(native_path, "__abi_check", module_owner)
        .err()
        .map(|e| e.to_string())
}

fn validate_built_native_extension_abi(
    native_path: &Path,
    module_dir: &Path,
) -> Result<(), ModuleSystemError> {
    let module_owner = module_owner_from_manifest_path(&module_dir.join("vo.mod"))?;
    if let Some(detail) = native_extension_host_abi_error(native_path, &module_owner) {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "native extension built at {} is not compatible with this Vo host ABI: {}",
                native_path.display(),
                detail,
            ),
        )
        .with_path(native_path));
    }
    Ok(())
}

fn write_native_extension_abi_marker(native_path: &Path) -> Result<(), ModuleSystemError> {
    let marker_path = native_extension_abi_marker_path(native_path);
    let marker = native_extension_abi_marker(native_path).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ReadFailed,
            format!(
                "failed to read native extension at {} while creating its ABI marker: {}",
                native_path.display(),
                e,
            ),
        )
        .with_path(native_path)
    })?;
    atomic_write_native_file(&marker_path, marker.as_bytes()).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "failed to write native extension ABI marker at {}: {}",
                marker_path.display(),
                e,
            ),
        )
        .with_path(&marker_path)
    })
}

#[cfg(test)]
pub(super) fn write_native_extension_test_abi_marker(
    native_path: &Path,
) -> Result<(), ModuleSystemError> {
    write_native_extension_abi_marker(native_path)
}

#[cfg(test)]
pub(super) fn write_native_extension_test_input_marker(
    native_path: &Path,
    module_dir: &Path,
) -> Result<(), ModuleSystemError> {
    let fingerprint = native_extension_input_fingerprint(module_dir)?;
    write_native_extension_input_marker(native_path, &fingerprint)
}

fn native_extension_abi_marker_path(native_path: &Path) -> PathBuf {
    let file_name = native_path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("native-extension");
    native_path.with_file_name(format!("{file_name}.vo-abi"))
}

fn current_native_extension_abi_identity() -> String {
    format!("version={ABI_VERSION}\nfingerprint={ABI_FINGERPRINT:#x}\n")
}

fn native_extension_abi_marker(native_path: &Path) -> Result<String, std::io::Error> {
    let bytes = read_bounded_file(
        native_path,
        NATIVE_EXTENSION_MAX_ARTIFACT_BYTES,
        "native extension artifact",
    )?;
    Ok(native_extension_abi_marker_for_bytes(&bytes))
}

fn native_extension_abi_marker_for_bytes(bytes: &[u8]) -> String {
    let mut hasher = StableHasher::new("vo-native-extension-artifact-v1");
    hasher.update_bytes("dylib", bytes);
    format!(
        "format={}\n{}artifact={}\n",
        NATIVE_EXTENSION_ABI_MARKER_FORMAT_VERSION,
        current_native_extension_abi_identity(),
        hasher.finish(),
    )
}

fn native_extension_input_marker_path(native_path: &Path) -> PathBuf {
    let file_name = native_path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("native-extension");
    native_path.with_file_name(format!("{file_name}.vo-inputs"))
}

fn native_extension_input_marker(input_fingerprint: &str) -> String {
    format!(
        "format={}\nfingerprint={}\n",
        NATIVE_EXTENSION_INPUT_MARKER_FORMAT_VERSION, input_fingerprint,
    )
}

fn write_native_extension_input_marker(
    native_path: &Path,
    input_fingerprint: &str,
) -> Result<(), ModuleSystemError> {
    let marker_path = native_extension_input_marker_path(native_path);
    let marker = native_extension_input_marker(input_fingerprint);
    atomic_write_native_file(&marker_path, marker.as_bytes()).map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "failed to write native extension input marker at {}: {}",
                marker_path.display(),
                error,
            ),
        )
        .with_path(&marker_path)
    })
}

fn atomic_write_native_file(path: &Path, bytes: &[u8]) -> Result<(), std::io::Error> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent)?;
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("native-extension-marker");
    let nonce = NATIVE_MARKER_TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_path = parent.join(format!(".{file_name}.tmp-{}-{nonce}", std::process::id(),));
    let result = (|| {
        let mut file = OpenOptions::new()
            .create_new(true)
            .write(true)
            .open(&temp_path)?;
        file.write_all(bytes)?;
        file.sync_all()?;
        replace_native_file(&temp_path, path)?;
        #[cfg(unix)]
        fs::File::open(parent)?.sync_all()?;
        Ok(())
    })();
    if result.is_err() {
        let _ = fs::remove_file(&temp_path);
    }
    result
}

#[cfg(not(windows))]
fn replace_native_file(temp_path: &Path, path: &Path) -> std::io::Result<()> {
    fs::rename(temp_path, path)
}

#[cfg(windows)]
fn replace_native_file(temp_path: &Path, path: &Path) -> std::io::Result<()> {
    match fs::rename(temp_path, path) {
        Ok(()) => Ok(()),
        Err(_error) if path.is_file() => {
            // Windows rename does not replace an existing destination. A
            // brief missing-marker window can only cause a conservative
            // rebuild; it cannot make a stale artifact valid.
            fs::remove_file(path)?;
            fs::rename(temp_path, path)
        }
        Err(error) => Err(error),
    }
}

fn native_extension_build_command(
    cargo: &NativeCargoContext,
    build_token: &str,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<std::process::Command, ModuleSystemError> {
    let mut command = std::process::Command::new("cargo");
    command
        .arg("build")
        .arg("--manifest-path")
        .arg(&cargo.manifest_path)
        .arg("--package")
        .arg(&cargo.root_package_name)
        .arg("--lib")
        .arg("--message-format=json-render-diagnostics");
    for config in &cargo.patch_configs {
        command.arg("--config").arg(config);
    }
    command.arg("--locked");
    if !cfg!(debug_assertions) {
        command.arg("--release");
    }
    command
        .current_dir(&cargo.rust_dir)
        .env(NATIVE_EXTENSION_FFI_SOURCE_FINGERPRINT_ENV, build_token)
        .env(
            "CARGO_ENCODED_RUSTFLAGS",
            native_extension_encoded_rustflags(build_token)?,
        )
        .env("CARGO_TARGET_DIR", &cargo.target_directory)
        .env_remove("CARGO_BUILD_TARGET")
        .env_remove("RUSTFLAGS")
        .env_remove("VO_JIT_CALL_THRESHOLD")
        .env_remove("VO_JIT_LOOP_THRESHOLD")
        .env_remove("VO_JIT_DEBUG");
    apply_workspace_discovery_to_command(&mut command, workspace_discovery);
    Ok(command)
}

fn native_extension_encoded_rustflags(build_token: &str) -> Result<OsString, ModuleSystemError> {
    const SEPARATOR: char = '\u{1f}';
    let mut arguments = if let Some(encoded) = std::env::var_os("CARGO_ENCODED_RUSTFLAGS") {
        encoded
    } else if let Some(rustflags) = std::env::var_os("RUSTFLAGS") {
        let rustflags = rustflags.into_string().map_err(|_| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                "RUSTFLAGS must be valid UTF-8 for deterministic native extension builds",
            )
        })?;
        rustflags
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(&SEPARATOR.to_string())
            .into()
    } else {
        OsString::new()
    };
    if !arguments.is_empty() {
        arguments.push(SEPARATOR.to_string());
    }
    arguments.push("--cfg");
    arguments.push(SEPARATOR.to_string());
    arguments.push(format!(
        "{NATIVE_EXTENSION_RUSTC_FINGERPRINT_CFG}={build_token:?}",
    ));
    Ok(arguments)
}

#[derive(Debug)]
struct BuiltCargoArtifact {
    path: PathBuf,
    fresh: bool,
}

#[derive(Debug, Deserialize)]
struct CargoCompilerArtifactMessage {
    reason: String,
    package_id: String,
    target: CargoCompilerArtifactTarget,
    filenames: Vec<PathBuf>,
    fresh: bool,
}

#[derive(Debug, Deserialize)]
struct CargoCompilerArtifactTarget {
    name: String,
    kind: Vec<String>,
    crate_types: Vec<String>,
}

fn build_native_extension(
    native_path: &Path,
    input_state: &NativeExtensionInputState,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<BuiltCargoArtifact, ModuleSystemError> {
    let rust_dir = &input_state.cargo.rust_dir;
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "native_extension_build_start")
            .path(rust_dir.display().to_string()),
    );
    let mut command = native_extension_build_command(
        &input_state.cargo,
        &input_state.build_token,
        workspace_discovery,
    )?;
    let lock_snapshot = CargoLockSnapshot::capture(&input_state.cargo.lock_path)?;
    let output_result = run_command_with_bounded_output(
        &mut command,
        NATIVE_EXTENSION_MAX_CARGO_BUILD_OUTPUT_BYTES,
    );
    let lock_check_result = lock_snapshot.verify_unchanged();
    let output = match output_result {
        Ok(output) => output,
        Err(e) => {
            lock_check_result?;
            return Err(ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::BuildFailed,
                format!("failed to run cargo build: {}", e),
            )
            .with_path(rust_dir));
        }
    };
    lock_check_result?;
    if output.stdout.len() > NATIVE_EXTENSION_MAX_CARGO_BUILD_OUTPUT_BYTES
        || output.stderr.len() > NATIVE_EXTENSION_MAX_CARGO_BUILD_OUTPUT_BYTES
    {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "cargo build output for {} exceeds the {}-byte limit",
                rust_dir.display(),
                NATIVE_EXTENSION_MAX_CARGO_BUILD_OUTPUT_BYTES,
            ),
        )
        .with_path(rust_dir));
    }
    if !output.status.success() {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "cargo build failed for {}: {}",
                rust_dir.display(),
                String::from_utf8_lossy(&output.stderr)
            ),
        )
        .with_path(rust_dir));
    }
    let artifact = parse_built_cargo_artifact(&output.stdout, &input_state.cargo, native_path)?;
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "native_extension_build_done")
            .path(rust_dir.display().to_string()),
    );
    Ok(artifact)
}

fn parse_built_cargo_artifact(
    stdout: &[u8],
    cargo: &NativeCargoContext,
    native_path: &Path,
) -> Result<BuiltCargoArtifact, ModuleSystemError> {
    let mut artifacts = BTreeMap::<PathBuf, bool>::new();
    for line in stdout.split(|byte| *byte == b'\n') {
        if line.is_empty() {
            continue;
        }
        let value = serde_json::from_slice::<serde_json::Value>(line).map_err(|error| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!("cargo emitted invalid JSON build output: {error}"),
            )
            .with_path(&cargo.manifest_path)
        })?;
        if value.get("reason").and_then(|value| value.as_str()) != Some("compiler-artifact") {
            continue;
        }
        let message: CargoCompilerArtifactMessage =
            serde_json::from_value(value).map_err(|error| {
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::ValidationFailed,
                    format!("invalid Cargo compiler-artifact message: {error}"),
                )
                .with_path(&cargo.manifest_path)
            })?;
        if message.reason != "compiler-artifact"
            || message.package_id != cargo.root_package_id
            || message.target.name != cargo.root_target_name
            || !(message.target.kind.iter().any(|kind| kind == "cdylib")
                || message
                    .target
                    .crate_types
                    .iter()
                    .any(|kind| kind == "cdylib"))
        {
            continue;
        }
        for filename in message.filenames {
            if is_platform_native_library(&filename) {
                artifacts
                    .entry(filename)
                    .and_modify(|fresh| *fresh &= message.fresh)
                    .or_insert(message.fresh);
            }
        }
    }
    if artifacts.len() != 1 {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "cargo emitted {} cdylib artifacts for native extension package {}; expected exactly one",
                artifacts.len(),
                cargo.root_package_name,
            ),
        )
        .with_path(&cargo.manifest_path));
    }
    let (artifact_path, fresh) = artifacts
        .into_iter()
        .next()
        .expect("exactly one Cargo artifact was checked");
    let artifact_path = artifact_path
        .canonicalize()
        .map_err(|error| native_build_input_read_error(&artifact_path, error))?;
    let declared_path = native_path.canonicalize().map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "declared [extension.native].path did not produce a library at {}: {} (Cargo produced {})",
                native_path.display(),
                error,
                artifact_path.display(),
            ),
        )
        .with_path(native_path)
    })?;
    if artifact_path != declared_path {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "Cargo produced native extension {} but [extension.native].path resolves to {}; align Cargo target-dir/target configuration with vo.mod",
                artifact_path.display(),
                declared_path.display(),
            ),
        )
        .with_path(native_path));
    }
    Ok(BuiltCargoArtifact {
        path: artifact_path,
        fresh,
    })
}

fn is_platform_native_library(path: &Path) -> bool {
    #[cfg(target_os = "linux")]
    const EXTENSION: &str = "so";
    #[cfg(target_os = "macos")]
    const EXTENSION: &str = "dylib";
    #[cfg(target_os = "windows")]
    const EXTENSION: &str = "dll";
    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    const EXTENSION: &str = "so";

    path.extension()
        .is_some_and(|extension| extension == EXTENSION)
}

#[derive(Debug)]
struct CargoLockSnapshot {
    path: PathBuf,
    bytes: Vec<u8>,
}

impl CargoLockSnapshot {
    fn capture(path: &Path) -> Result<Self, ModuleSystemError> {
        let path = path.to_path_buf();
        let bytes =
            read_bounded_file(&path, MAX_TEXT_FILE_BYTES, "Cargo lockfile").map_err(|e| {
                let kind = if e.kind() == std::io::ErrorKind::NotFound {
                    ModuleSystemErrorKind::Missing
                } else {
                    ModuleSystemErrorKind::ReadFailed
                };
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    kind,
                    format!(
                        "native extension builds require a checked-in Cargo.lock at {}: {}",
                        path.display(),
                        e,
                    ),
                )
                .with_path(&path)
            })?;
        Ok(Self { path, bytes })
    }

    fn verify_unchanged(&self) -> Result<(), ModuleSystemError> {
        let current = read_bounded_file(&self.path, MAX_TEXT_FILE_BYTES, "Cargo lockfile")
            .map_err(|e| {
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::BuildFailed,
                    format!(
                        "failed to verify Cargo.lock after native extension build at {}: {}",
                        self.path.display(),
                        e,
                    ),
                )
                .with_path(&self.path)
            })?;
        if current == self.bytes {
            return Ok(());
        }
        Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "cargo changed Cargo.lock during a locked native extension build at {}",
                self.path.display(),
            ),
        )
        .with_path(&self.path))
    }
}

fn local_volang_cargo_patch_configs() -> Vec<String> {
    let Some((_crates_root, packages)) = local_volang_packages() else {
        return Vec::new();
    };

    let sources = [
        "https://github.com/vo-lang/volang",
        "https://github.com/vo-lang/volang.git",
    ];
    let mut configs = Vec::new();
    for source in sources {
        for (package, path) in &packages {
            configs.push(format!(
                "patch.{}.{}.path={}",
                toml_quote(source),
                toml_quote(package),
                toml_quote(&path.to_string_lossy()),
            ));
        }
    }
    configs
}

fn local_volang_packages() -> Option<(PathBuf, Vec<(&'static str, PathBuf)>)> {
    // Development binaries usually live below `<checkout>/target`. A custom
    // CARGO_TARGET_DIR can place them elsewhere, so fall back to the command's
    // working-directory ancestors. Both searches happen at runtime, keeping
    // the binary identity independent of the build machine's checkout path.
    let executable = std::env::current_exe().ok()?.canonicalize().ok()?;
    let is_volang_root = |candidate: &Path| {
        candidate.join("Cargo.toml").is_file()
            && candidate.join("lang/crates/vo-engine/Cargo.toml").is_file()
            && candidate.join("lang/stdlib/Cargo.toml").is_file()
    };
    let volang_root = executable
        .ancestors()
        .skip(1)
        .find(|candidate| is_volang_root(candidate))
        .map(Path::to_path_buf)
        .or_else(|| {
            std::env::current_dir()
                .ok()?
                .canonicalize()
                .ok()?
                .ancestors()
                .find(|candidate| is_volang_root(candidate))
                .map(Path::to_path_buf)
        })?;
    let crates_root = volang_root.join("lang/crates");
    let lang_root = volang_root.join("lang");
    let packages = vec![
        ("vo-analysis", crates_root.join("vo-analysis")),
        ("vo-app-runtime", crates_root.join("vo-app-runtime")),
        ("vo-codegen", crates_root.join("vo-codegen")),
        ("vo-common", crates_root.join("vo-common")),
        ("vo-common-core", crates_root.join("vo-common-core")),
        ("vo-engine", crates_root.join("vo-engine")),
        ("vo-ext", crates_root.join("vo-ext")),
        ("vo-ffi-macro", crates_root.join("vo-ffi-macro")),
        ("vo-jit", crates_root.join("vo-jit")),
        ("vo-module", crates_root.join("vo-module")),
        ("vo-runtime", crates_root.join("vo-runtime")),
        ("vo-stdlib", crates_root.join("vo-stdlib")),
        ("vo-stdlib-source", lang_root.join("stdlib")),
        ("vo-syntax", crates_root.join("vo-syntax")),
        ("vo-vm", crates_root.join("vo-vm")),
        ("vo-web", crates_root.join("vo-web")),
    ];
    if packages
        .iter()
        .any(|(_, path)| !path.join("Cargo.toml").is_file())
    {
        return None;
    }
    Some((crates_root, packages))
}

fn toml_quote(value: &str) -> String {
    let escaped = value.replace('\\', "\\\\").replace('"', "\\\"");
    format!("\"{}\"", escaped)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
    use std::sync::Barrier;
    use std::time::SystemTime;
    use vo_module::ext_manifest::{
        ExtensionManifest, NativeExtensionConfig, NativeTargetDeclaration,
    };

    fn temp_dir(prefix: &str) -> PathBuf {
        std::env::temp_dir().join(format!(
            "{}_{}_{}",
            prefix,
            std::process::id(),
            SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ))
    }

    fn current_platform_library_name(stem: &str) -> String {
        #[cfg(target_os = "linux")]
        {
            format!("{}.so", stem)
        }
        #[cfg(target_os = "macos")]
        {
            format!("{}.dylib", stem)
        }
        #[cfg(target_os = "windows")]
        {
            format!("{}.dll", stem.strip_prefix("lib").unwrap_or(stem))
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
        {
            format!("{}.so", stem)
        }
    }

    fn local_manifest(module_dir: &Path) -> ExtensionManifest {
        ExtensionManifest {
            name: "demo".to_string(),
            include: Vec::new(),
            native: Some(NativeExtensionConfig {
                path: Some("rust/target/{profile}/libdemo".to_string()),
                targets: vec![NativeTargetDeclaration {
                    target: current_target_triple().to_string(),
                    library: current_platform_library_name("libdemo"),
                }],
            }),
            wasm: None,
            web: None,
            manifest_path: module_dir.join("vo.mod"),
        }
    }

    fn write_native_input_fixture(module_dir: &Path) {
        let rust_dir = module_dir.join("rust");
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(rust_dir.join("Cargo.lock"), b"# lock\n").unwrap();
        fs::write(rust_dir.join("build.rs"), b"fn main() {}\n").unwrap();
        fs::write(rust_dir.join("src/lib.rs"), b"pub fn value() -> u8 { 1 }\n").unwrap();
        fs::write(rust_dir.join("build-input.bin"), [0, 1, 2, 255]).unwrap();
    }

    fn write_local_manifest(module_dir: &Path) {
        fs::write(
            module_dir.join("vo.mod"),
            format!(
                concat!(
                    "module github.com/example/demo\n",
                    "vo ^0.1.0\n\n",
                    "[extension]\n",
                    "name = \"demo\"\n\n",
                    "[extension.native]\n",
                    "path = \"rust/target/{{profile}}/libdemo\"\n\n",
                    "[[extension.native.targets]]\n",
                    "target = \"{}\"\n",
                    "library = \"{}\"\n",
                ),
                current_target_triple(),
                current_platform_library_name("libdemo"),
            ),
        )
        .unwrap();
    }

    fn write_module_source_fixture(module_dir: &Path) {
        fs::create_dir_all(module_dir).unwrap();
        write_local_manifest(module_dir);
        fs::write(
            module_dir.join("api.vo"),
            b"package demo\n\nextern func Value() int\n",
        )
        .unwrap();
        fs::write(module_dir.join("vo.lock"), b"version = 2\n").unwrap();
    }

    fn generate_fixture_cargo_lock(manifest_path: &Path) {
        let output = std::process::Command::new("cargo")
            .arg("generate-lockfile")
            .arg("--offline")
            .arg("--manifest-path")
            .arg(manifest_path)
            .env_remove("VOWORK")
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "failed to generate fixture Cargo.lock: {}",
            String::from_utf8_lossy(&output.stderr),
        );
    }

    fn write_simple_cdylib_fixture(module_dir: &Path, lib_source: &str) -> PathBuf {
        write_module_source_fixture(module_dir);
        let rust_dir = module_dir.join("rust");
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            concat!(
                "[package]\n",
                "name = \"demo\"\n",
                "version = \"0.1.0\"\n",
                "edition = \"2021\"\n\n",
                "[lib]\n",
                "crate-type = [\"cdylib\"]\n",
            ),
        )
        .unwrap();
        fs::write(rust_dir.join("src/lib.rs"), lib_source).unwrap();
        generate_fixture_cargo_lock(&rust_dir.join("Cargo.toml"));
        local_manifest(module_dir)
            .resolve_local_native_path(module_dir)
            .unwrap()
            .unwrap()
    }

    fn abi_native_extension_source(source_marker: u8) -> String {
        format!(
            concat!(
                "#[repr(C)]\n",
                "pub struct ExtensionTable {{\n",
                "    pub version: u32,\n",
                "    pub entry_count: u32,\n",
                "    pub entries: *const u8,\n",
                "}}\n\n",
                "#[no_mangle]\n",
                "pub extern \"C\" fn vo_ext_get_abi_version() -> u32 {{ {abi_version} }}\n\n",
                "#[no_mangle]\n",
                "pub extern \"C\" fn vo_ext_get_abi_fingerprint() -> u64 {{ {abi_fingerprint} }}\n\n",
                "#[no_mangle]\n",
                "pub extern \"C\" fn vo_ext_get_entries() -> ExtensionTable {{\n",
                "    ExtensionTable {{\n",
                "        version: {abi_version},\n",
                "        entry_count: 0,\n",
                "        entries: std::ptr::null(),\n",
                "    }}\n",
                "}}\n\n",
                "#[no_mangle]\n",
                "pub static SOURCE_MARKER: u8 = {source_marker};\n",
            ),
            abi_version = ABI_VERSION,
            abi_fingerprint = ABI_FINGERPRINT,
            source_marker = source_marker,
        )
    }

    fn write_abi_native_extension_fixture(module_dir: &Path) -> PathBuf {
        write_simple_cdylib_fixture(module_dir, &abi_native_extension_source(1))
    }

    fn test_native_extension_input_fingerprint(
        module_dir: &Path,
        workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    ) -> Result<String, ModuleSystemError> {
        compute_native_extension_input_fingerprint(module_dir, workspace_discovery, || {
            Ok("test-build-context".to_string())
        })
    }

    fn prime_native_extension_input_cache(module_dir: &Path, fingerprint: &str) -> PathBuf {
        let native_path = local_manifest(module_dir)
            .resolve_local_native_path(module_dir)
            .unwrap()
            .unwrap();
        fs::create_dir_all(native_path.parent().unwrap()).unwrap();
        fs::write(&native_path, b"artifact").unwrap();
        write_native_extension_abi_marker(&native_path).unwrap();
        write_native_extension_input_marker(&native_path, fingerprint).unwrap();
        native_path
    }

    #[test]
    fn native_extension_specs_dedupe_duplicate_module_manifests() {
        let root = temp_dir("vo_native_ext_dedupe");
        let mod_root = root.join("mod-cache");
        let module_dir = root.join("demo");
        fs::create_dir_all(&module_dir).unwrap();
        fs::create_dir_all(&mod_root).unwrap();
        write_local_manifest(&module_dir);

        let manifest = local_manifest(&module_dir);
        let specs = prepare_native_extension_specs_with_readiness(
            &[manifest.clone(), manifest],
            &[],
            &mod_root,
        )
        .unwrap();

        assert_eq!(
            specs.len(),
            1,
            "duplicate extension specs should not reload one dylib twice"
        );
        assert_eq!(specs[0].name, "demo");
        assert_eq!(specs[0].module_owner, "github.com/example/demo");

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_input_fingerprint_tracks_content_without_timestamp_assumptions() {
        let root = temp_dir("vo_native_input_content");
        let module_dir = root.join("demo");
        write_native_input_fixture(&module_dir);
        write_local_manifest(&module_dir);
        fs::create_dir_all(root.join(".cargo")).unwrap();
        fs::write(root.join(".cargo/config.toml"), b"[net]\noffline = true\n").unwrap();

        let source_path = module_dir.join("rust/src/lib.rs");
        let original_modified = fs::metadata(&source_path).unwrap().modified().unwrap();
        let original = native_extension_input_fingerprint(&module_dir).unwrap();
        fs::write(&source_path, b"pub fn value() -> u8 { 2 }\n").unwrap();
        fs::OpenOptions::new()
            .write(true)
            .open(&source_path)
            .unwrap()
            .set_times(fs::FileTimes::new().set_modified(original_modified))
            .unwrap();
        let source_changed = native_extension_input_fingerprint(&module_dir).unwrap();
        assert_ne!(original, source_changed);

        fs::write(root.join(".cargo/config.toml"), b"[net]\noffline = false\n").unwrap();
        let config_changed = native_extension_input_fingerprint(&module_dir).unwrap();
        assert_ne!(source_changed, config_changed);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn local_native_cache_tracks_module_source_membership_and_contents() {
        let root = temp_dir("vo_native_module_inputs");
        let module_dir = root.join("demo");
        write_native_input_fixture(&module_dir);
        write_module_source_fixture(&module_dir);
        let workspace_discovery = vo_module::workspace::WorkspaceDiscovery::Disabled;
        let original_fingerprint =
            test_native_extension_input_fingerprint(&module_dir, &workspace_discovery).unwrap();
        let native_path = prime_native_extension_input_cache(&module_dir, &original_fingerprint);
        let cache_is_current = || {
            let fingerprint =
                test_native_extension_input_fingerprint(&module_dir, &workspace_discovery).unwrap();
            native_extension_cache_is_valid_for_fingerprint(&native_path, &fingerprint).unwrap()
        };
        assert!(cache_is_current());

        let added_source = module_dir.join("added.vo");
        fs::write(&added_source, b"package demo\n").unwrap();
        assert!(!cache_is_current());
        fs::remove_file(&added_source).unwrap();
        assert!(cache_is_current());

        let source_path = module_dir.join("api.vo");
        let source_bytes = fs::read(&source_path).unwrap();
        let source_modified = fs::metadata(&source_path).unwrap().modified().unwrap();
        fs::write(&source_path, b"package demo\n\nextern func Other() int\n").unwrap();
        fs::OpenOptions::new()
            .write(true)
            .open(&source_path)
            .unwrap()
            .set_times(fs::FileTimes::new().set_modified(source_modified))
            .unwrap();
        assert!(!cache_is_current());
        fs::write(&source_path, &source_bytes).unwrap();
        assert!(cache_is_current());

        fs::remove_file(&source_path).unwrap();
        assert!(!cache_is_current());
        fs::write(&source_path, &source_bytes).unwrap();
        assert!(cache_is_current());

        let manifest_path = module_dir.join("vo.mod");
        let manifest_bytes = fs::read(&manifest_path).unwrap();
        let manifest_modified = fs::metadata(&manifest_path).unwrap().modified().unwrap();
        let mut changed_manifest = manifest_bytes.clone();
        changed_manifest.extend_from_slice(b"\n# source membership changed\n");
        fs::write(&manifest_path, changed_manifest).unwrap();
        fs::OpenOptions::new()
            .write(true)
            .open(&manifest_path)
            .unwrap()
            .set_times(fs::FileTimes::new().set_modified(manifest_modified))
            .unwrap();
        assert!(!cache_is_current());
        fs::write(&manifest_path, manifest_bytes).unwrap();
        assert!(cache_is_current());

        let lock_path = module_dir.join("vo.lock");
        fs::write(&lock_path, b"version = 2\n# changed\n").unwrap();
        assert!(!cache_is_current());
        fs::write(&lock_path, b"version = 2\n").unwrap();
        assert!(cache_is_current());

        for ignored_dir in [module_dir.join("target"), module_dir.join(".git")] {
            fs::create_dir_all(&ignored_dir).unwrap();
            fs::write(ignored_dir.join("ignored.vo"), b"package ignored\n").unwrap();
        }
        assert!(cache_is_current());

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_input_fingerprint_tracks_active_workspace_sources() {
        let root = temp_dir("vo_native_workspace_inputs");
        let workspace = root.join("workspace");
        let module_dir = workspace.join("app");
        let dependency_dir = workspace.join("dependency");
        write_native_input_fixture(&module_dir);
        write_module_source_fixture(&module_dir);
        fs::create_dir_all(&dependency_dir).unwrap();
        fs::write(
            dependency_dir.join("vo.mod"),
            b"module github.com/example/dependency\nvo ^0.1.0\n",
        )
        .unwrap();
        fs::write(
            dependency_dir.join("types.vo"),
            b"package dependency\n\ntype Value int\n",
        )
        .unwrap();
        let workfile = workspace.join("vo.work");
        fs::write(
            &workfile,
            b"version = 1\n\n[[use]]\npath = \"dependency\"\n",
        )
        .unwrap();
        let discovery = vo_module::workspace::WorkspaceDiscovery::Explicit(workfile.clone());

        let original = test_native_extension_input_fingerprint(&module_dir, &discovery).unwrap();
        fs::write(
            dependency_dir.join("types.vo"),
            b"package dependency\n\ntype Value string\n",
        )
        .unwrap();
        let dependency_changed =
            test_native_extension_input_fingerprint(&module_dir, &discovery).unwrap();
        assert_ne!(original, dependency_changed);

        let added_source = dependency_dir.join("added.vo");
        fs::write(&added_source, b"package dependency\n").unwrap();
        let membership_changed =
            test_native_extension_input_fingerprint(&module_dir, &discovery).unwrap();
        assert_ne!(dependency_changed, membership_changed);
        fs::remove_file(added_source).unwrap();

        fs::write(
            &workfile,
            b"version = 1\n\n[[use]]\npath = \"dependency\"\n# changed\n",
        )
        .unwrap();
        let workfile_changed =
            test_native_extension_input_fingerprint(&module_dir, &discovery).unwrap();
        assert_ne!(dependency_changed, workfile_changed);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_input_fingerprint_is_path_order_stable() {
        let rust_dir = Path::new("/normalized/module/rust");
        let forward = vec![
            (PathBuf::from("Cargo.toml"), b"manifest".to_vec()),
            (PathBuf::from("src/lib.rs"), b"source".to_vec()),
            (PathBuf::from("assets/data.bin"), vec![3, 2, 1]),
        ];
        let mut reverse = forward.clone();
        reverse.reverse();
        let module_forward = vec![
            (PathBuf::from("/normalized/module/api.vo"), b"api".to_vec()),
            (
                PathBuf::from("/normalized/workspace/vo.work"),
                b"workspace".to_vec(),
            ),
        ];
        let mut module_reverse = module_forward.clone();
        module_reverse.reverse();

        assert_eq!(
            hash_native_build_inputs(
                rust_dir,
                &forward,
                &module_forward,
                &vo_module::workspace::WorkspaceDiscovery::Auto,
                "context",
            ),
            hash_native_build_inputs(
                rust_dir,
                &reverse,
                &module_reverse,
                &vo_module::workspace::WorkspaceDiscovery::Auto,
                "context",
            ),
        );
        assert_ne!(
            hash_native_build_inputs(
                rust_dir,
                &forward,
                &module_forward,
                &vo_module::workspace::WorkspaceDiscovery::Auto,
                "context",
            ),
            hash_native_build_inputs(
                rust_dir,
                &forward,
                &module_forward,
                &vo_module::workspace::WorkspaceDiscovery::Disabled,
                "context",
            ),
        );
    }

    #[test]
    fn command_output_is_rejected_at_the_reader_limit() {
        let mut command = std::process::Command::new(std::env::current_exe().unwrap());
        command.arg("--list");

        let error = run_command_with_bounded_output(&mut command, 8).unwrap_err();
        assert_eq!(error.kind(), std::io::ErrorKind::InvalidData);
        assert!(error.to_string().contains("8-byte limit"));
    }

    #[test]
    fn explicit_workspace_is_canonicalized_before_cargo_invocation() {
        let root = temp_dir("vo_native_explicit_workspace");
        let module_dir = root.join("module");
        fs::create_dir_all(&module_dir).unwrap();
        let workspace_file = root.join("selected.vo.work");
        fs::write(&workspace_file, b"version = 1\n").unwrap();

        let discovery = normalized_workspace_discovery(
            &module_dir,
            &vo_module::workspace::WorkspaceDiscovery::Explicit(PathBuf::from(
                "../selected.vo.work",
            )),
        )
        .unwrap();
        let vo_module::workspace::WorkspaceDiscovery::Explicit(selected) = &discovery else {
            panic!("expected explicit workspace discovery");
        };
        assert_eq!(selected, &workspace_file.canonicalize().unwrap());

        let mut command = std::process::Command::new("cargo");
        apply_workspace_discovery_to_command(&mut command, &discovery);
        assert!(command.get_envs().any(|(key, value)| {
            key == std::ffi::OsStr::new("VOWORK") && value == Some(selected.as_os_str())
        }));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn native_extension_build_command_sets_current_ffi_source_fingerprint() {
        let rust_dir = PathBuf::from("/tmp/vo-native-command/rust");
        let fingerprint = "sha256:current-source";
        let cargo = NativeCargoContext {
            manifest_path: rust_dir.join("Cargo.toml"),
            workspace_root: rust_dir.clone(),
            lock_path: rust_dir.join("Cargo.lock"),
            target_directory: rust_dir.join("target"),
            root_package_id: "path+file:///tmp/demo#0.1.0".to_string(),
            root_package_name: "demo".to_string(),
            root_target_name: "demo".to_string(),
            local_package_roots: vec![rust_dir.clone()],
            patch_configs: Vec::new(),
            metadata_digest: "metadata".to_string(),
            rust_dir: rust_dir.clone(),
        };
        let command = native_extension_build_command(
            &cargo,
            fingerprint,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();
        let configured = command
            .get_envs()
            .find(|(key, _)| {
                *key == std::ffi::OsStr::new(NATIVE_EXTENSION_FFI_SOURCE_FINGERPRINT_ENV)
            })
            .and_then(|(_, value)| value);

        assert_eq!(configured, Some(std::ffi::OsStr::new(fingerprint)));
        assert_eq!(command.get_current_dir(), Some(rust_dir.as_path()));
        assert_eq!(
            command.get_args().next(),
            Some(std::ffi::OsStr::new("build"))
        );
        assert!(command
            .get_args()
            .any(|argument| argument == std::ffi::OsStr::new("--locked")));
        assert!(command.get_envs().any(|(key, value)| {
            key == std::ffi::OsStr::new("VOWORK") && value == Some(std::ffi::OsStr::new("off"))
        }));
        assert!(command.get_envs().any(|(key, value)| {
            key == std::ffi::OsStr::new("CARGO_ENCODED_RUSTFLAGS")
                && value.is_some_and(|value| {
                    value
                        .to_string_lossy()
                        .contains(NATIVE_EXTENSION_RUSTC_FINGERPRINT_CFG)
                })
        }));

        let auto = native_extension_build_command(
            &cargo,
            fingerprint,
            &vo_module::workspace::WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert!(auto
            .get_envs()
            .any(|(key, value)| { key == std::ffi::OsStr::new("VOWORK") && value.is_none() }));

        let selected = PathBuf::from("/tmp/selected.vo.work");
        let explicit = native_extension_build_command(
            &cargo,
            fingerprint,
            &vo_module::workspace::WorkspaceDiscovery::Explicit(selected.clone()),
        )
        .unwrap();
        assert!(explicit.get_envs().any(|(key, value)| {
            key == std::ffi::OsStr::new("VOWORK") && value == Some(selected.as_os_str())
        }));
    }

    #[test]
    fn cargo_metadata_uses_the_actual_ancestor_workspace_lock() {
        let root = temp_dir("vo_native_ancestor_cargo_workspace");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        write_module_source_fixture(&module_dir);
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::write(
            root.join("Cargo.toml"),
            "[workspace]\nresolver = \"2\"\nmembers = [\"module/rust\"]\n",
        )
        .unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            concat!(
                "[package]\n",
                "name = \"demo\"\n",
                "version = \"0.1.0\"\n",
                "edition = \"2021\"\n\n",
                "[lib]\n",
                "crate-type = [\"cdylib\"]\n",
            ),
        )
        .unwrap();
        fs::write(rust_dir.join("src/lib.rs"), abi_native_extension_source(1)).unwrap();
        generate_fixture_cargo_lock(&rust_dir.join("Cargo.toml"));
        let lock_before = fs::read(root.join("Cargo.lock")).unwrap();
        let native_path = local_manifest(&module_dir)
            .resolve_local_native_path(&module_dir)
            .unwrap()
            .unwrap();

        let cargo = inspect_native_cargo_context(
            &module_dir,
            &native_path,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();
        assert_eq!(cargo.workspace_root, root.canonicalize().unwrap());
        assert_eq!(
            cargo.lock_path,
            root.join("Cargo.lock").canonicalize().unwrap(),
        );
        assert!(cargo.lock_path.is_file());
        assert!(!rust_dir.join("Cargo.lock").exists());

        ensure_local_native_extension_built_with_workspace(
            &module_dir,
            &native_path,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();
        assert_eq!(fs::read(root.join("Cargo.lock")).unwrap(), lock_before);
        assert!(!rust_dir.join("Cargo.lock").exists());

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn compiler_artifact_must_match_the_declared_native_path() {
        let root = temp_dir("vo_native_artifact_binding");
        let rust_dir = root.join("rust");
        let actual = rust_dir
            .join("other-target/debug")
            .join(current_platform_library_name("libdemo"));
        let declared = rust_dir
            .join("target/debug")
            .join(current_platform_library_name("libdemo"));
        fs::create_dir_all(actual.parent().unwrap()).unwrap();
        fs::create_dir_all(declared.parent().unwrap()).unwrap();
        fs::write(&actual, b"new cargo artifact").unwrap();
        fs::write(&declared, b"stale declared artifact").unwrap();
        let package_id = "path+file:///tmp/demo#0.1.0";
        let cargo = NativeCargoContext {
            rust_dir: rust_dir.clone(),
            manifest_path: rust_dir.join("Cargo.toml"),
            workspace_root: rust_dir.clone(),
            lock_path: rust_dir.join("Cargo.lock"),
            target_directory: rust_dir.join("other-target"),
            root_package_id: package_id.to_string(),
            root_package_name: "demo".to_string(),
            root_target_name: "demo".to_string(),
            local_package_roots: vec![rust_dir.clone()],
            patch_configs: Vec::new(),
            metadata_digest: "metadata".to_string(),
        };
        let output = serde_json::to_vec(&serde_json::json!({
            "reason": "compiler-artifact",
            "package_id": package_id,
            "target": {
                "name": "demo",
                "kind": ["cdylib"],
                "crate_types": ["cdylib"],
            },
            "filenames": [actual],
            "fresh": false,
        }))
        .unwrap();

        let error = parse_built_cargo_artifact(&output, &cargo, &declared).unwrap_err();
        assert_eq!(error.kind, ModuleSystemErrorKind::BuildFailed);
        assert!(error.detail.contains("Cargo produced native extension"));

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn path_dependency_content_change_with_same_mtime_rebuilds_the_cdylib() {
        let root = temp_dir("vo_native_path_dep_same_mtime");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        let dependency_dir = root.join("dep");
        write_module_source_fixture(&module_dir);
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::create_dir_all(dependency_dir.join("src")).unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            concat!(
                "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n",
                "[lib]\ncrate-type = [\"cdylib\"]\n\n",
                "[dependencies]\ndep = { path = \"../../dep\" }\n",
            ),
        )
        .unwrap();
        fs::write(
            rust_dir.join("src/lib.rs"),
            "#[no_mangle]\npub extern \"C\" fn demo_value() -> u64 { dep::value() }\n",
        )
        .unwrap();
        fs::write(
            dependency_dir.join("Cargo.toml"),
            "[package]\nname = \"dep\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        )
        .unwrap();
        let dependency_source = dependency_dir.join("src/lib.rs");
        fs::write(
            &dependency_source,
            "#[inline(never)]\npub fn value() -> u64 { 1 }\n",
        )
        .unwrap();
        generate_fixture_cargo_lock(&rust_dir.join("Cargo.toml"));
        let native_path = local_manifest(&module_dir)
            .resolve_local_native_path(&module_dir)
            .unwrap()
            .unwrap();
        let discovery = vo_module::workspace::WorkspaceDiscovery::Disabled;

        let initial =
            capture_native_extension_input_state(&module_dir, &native_path, &discovery).unwrap();
        build_native_extension(&native_path, &initial, &discovery).unwrap();
        let first =
            capture_native_extension_input_state(&module_dir, &native_path, &discovery).unwrap();
        let first_artifact = build_native_extension(&native_path, &first, &discovery).unwrap();
        let first_bytes = fs::read(&first_artifact.path).unwrap();
        let modified = fs::metadata(&dependency_source)
            .unwrap()
            .modified()
            .unwrap();
        fs::write(
            &dependency_source,
            "#[inline(never)]\npub fn value() -> u64 { 2 }\n",
        )
        .unwrap();
        fs::OpenOptions::new()
            .write(true)
            .open(&dependency_source)
            .unwrap()
            .set_times(fs::FileTimes::new().set_modified(modified))
            .unwrap();

        let second =
            capture_native_extension_input_state(&module_dir, &native_path, &discovery).unwrap();
        assert_ne!(first.fingerprint, second.fingerprint);
        assert_ne!(first.build_token, second.build_token);
        let second_artifact = build_native_extension(&native_path, &second, &discovery).unwrap();
        let second_bytes = fs::read(&second_artifact.path).unwrap();
        assert!(!second_artifact.fresh);
        assert_ne!(first_bytes, second_bytes);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn always_build_honors_external_build_script_rerun_inputs() {
        let root = temp_dir("vo_native_external_rerun_input");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        let external = root.join("external.txt");
        write_module_source_fixture(&module_dir);
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::write(&external, b"1\n").unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            concat!(
                "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
                "build = \"build.rs\"\n\n",
                "[lib]\ncrate-type = [\"cdylib\"]\n",
            ),
        )
        .unwrap();
        fs::write(
            rust_dir.join("build.rs"),
            concat!(
                "fn main() {\n",
                "    println!(\"cargo:rerun-if-changed=../../external.txt\");\n",
                "    let value = std::fs::read_to_string(\"../../external.txt\").unwrap();\n",
                "    let value: u64 = value.trim().parse().unwrap();\n",
                "    let out = std::path::PathBuf::from(std::env::var_os(\"OUT_DIR\").unwrap());\n",
                "    std::fs::write(out.join(\"generated.rs\"), format!(\"pub const VALUE: u64 = {value};\\n\")).unwrap();\n",
                "}\n",
            ),
        )
        .unwrap();
        fs::write(
            rust_dir.join("src/lib.rs"),
            concat!(
                "include!(concat!(env!(\"OUT_DIR\"), \"/generated.rs\"));\n",
                "#[no_mangle]\npub extern \"C\" fn demo_value() -> u64 { VALUE }\n",
            ),
        )
        .unwrap();
        generate_fixture_cargo_lock(&rust_dir.join("Cargo.toml"));
        let native_path = local_manifest(&module_dir)
            .resolve_local_native_path(&module_dir)
            .unwrap()
            .unwrap();
        let discovery = vo_module::workspace::WorkspaceDiscovery::Disabled;

        let initial =
            capture_native_extension_input_state(&module_dir, &native_path, &discovery).unwrap();
        build_native_extension(&native_path, &initial, &discovery).unwrap();
        let first =
            capture_native_extension_input_state(&module_dir, &native_path, &discovery).unwrap();
        let first_artifact = build_native_extension(&native_path, &first, &discovery).unwrap();
        let first_bytes = fs::read(&first_artifact.path).unwrap();
        let future = SystemTime::now() + std::time::Duration::from_secs(2);
        fs::write(&external, b"2\n").unwrap();
        fs::OpenOptions::new()
            .write(true)
            .open(&external)
            .unwrap()
            .set_times(fs::FileTimes::new().set_modified(future))
            .unwrap();

        let second =
            capture_native_extension_input_state(&module_dir, &native_path, &discovery).unwrap();
        assert_eq!(first.fingerprint, second.fingerprint);
        assert_eq!(first.build_token, second.build_token);
        let second_artifact = build_native_extension(&native_path, &second, &discovery).unwrap();
        let second_bytes = fs::read(&second_artifact.path).unwrap();
        assert!(!second_artifact.fresh);
        assert_ne!(first_bytes, second_bytes);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn a_b_a_changes_during_build_are_rejected_and_force_new_generations() {
        let root = temp_dir("vo_native_build_aba");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        let native_path = write_simple_cdylib_fixture(
            &module_dir,
            concat!(
                "include!(concat!(env!(\"OUT_DIR\"), \"/generated.rs\"));\n",
                "#[no_mangle]\npub extern \"C\" fn demo_value() -> u64 { VALUE }\n",
            ),
        );
        fs::write(module_dir.join("aba.txt"), b"A\n").unwrap();
        fs::write(
            rust_dir.join("build.rs"),
            concat!(
                "fn main() {\n",
                "    println!(\"cargo:rerun-if-changed=../aba.txt\");\n",
                "    println!(\"cargo:rerun-if-env-changed=VO_FFI_SOURCE_FINGERPRINT\");\n",
                "    let token = std::env::var(\"VO_FFI_SOURCE_FINGERPRINT\").unwrap();\n",
                "    let mut tokens = std::fs::OpenOptions::new().create(true).append(true).open(\"../../tokens.txt\").unwrap();\n",
                "    use std::io::Write as _;\n",
                "    writeln!(tokens, \"{token}\").unwrap();\n",
                "    let path = std::path::Path::new(\"../aba.txt\");\n",
                "    let original = std::fs::read(path).unwrap();\n",
                "    std::fs::write(path, b\"B\\n\").unwrap();\n",
                "    let out = std::path::PathBuf::from(std::env::var_os(\"OUT_DIR\").unwrap());\n",
                "    std::fs::write(out.join(\"generated.rs\"), b\"pub const VALUE: u64 = 2;\\n\").unwrap();\n",
                "    std::fs::write(path, original).unwrap();\n",
                "}\n",
            ),
        )
        .unwrap();

        let error = ensure_local_native_extension_built_with_workspace(
            &module_dir,
            &native_path,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap_err();
        assert_eq!(error.kind, ModuleSystemErrorKind::BuildFailed);
        assert!(
            error.detail.contains("inputs kept changing"),
            "{}",
            error.detail,
        );
        assert_eq!(fs::read(module_dir.join("aba.txt")).unwrap(), b"A\n");
        let tokens = fs::read_to_string(root.join("tokens.txt")).unwrap();
        let tokens = tokens.lines().collect::<BTreeSet<_>>();
        assert_eq!(tokens.len(), NATIVE_EXTENSION_BUILD_ATTEMPTS);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_extension_build_requires_an_immutable_cargo_lock() {
        let root = temp_dir("vo_native_cargo_lock");
        let rust_dir = root.join("rust");
        fs::create_dir_all(&rust_dir).unwrap();

        let lock_path = rust_dir.join("Cargo.lock");
        let missing = CargoLockSnapshot::capture(&lock_path).unwrap_err();
        assert_eq!(missing.kind, ModuleSystemErrorKind::Missing);

        fs::write(&lock_path, b"version = 4\n").unwrap();
        let snapshot = CargoLockSnapshot::capture(&lock_path).unwrap();
        snapshot.verify_unchanged().unwrap();
        fs::write(&lock_path, b"version = 4\n# changed\n").unwrap();
        let changed = snapshot.verify_unchanged().unwrap_err();
        assert_eq!(changed.kind, ModuleSystemErrorKind::BuildFailed);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn local_volang_patch_context_includes_stdlib_source_package() {
        let (_crates_root, packages) = local_volang_packages().unwrap();
        let source_root = packages
            .iter()
            .find_map(|(name, path)| (*name == "vo-stdlib-source").then_some(path))
            .expect("vo-stdlib-source must be patched into native extension builds");

        assert_eq!(
            source_root.file_name(),
            Some(std::ffi::OsStr::new("stdlib"))
        );
        assert!(source_root.join("Cargo.toml").is_file());
        assert!(local_volang_cargo_patch_configs()
            .iter()
            .any(|config| config.contains("vo-stdlib-source")));
    }

    #[test]
    fn native_input_fingerprint_rejects_oversized_files_before_reading_them() {
        let root = temp_dir("vo_native_oversized_input");
        let module_dir = root.join("demo");
        write_native_input_fixture(&module_dir);
        write_local_manifest(&module_dir);
        let path = module_dir.join("rust/oversized.bin");
        fs::File::create(&path)
            .unwrap()
            .set_len(u64::try_from(NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES).unwrap() + 1)
            .unwrap();

        let error = native_extension_input_fingerprint(&module_dir).unwrap_err();
        assert_eq!(error.kind, ModuleSystemErrorKind::ReadFailed);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_input_fingerprint_rejects_oversized_module_sources() {
        let root = temp_dir("vo_native_oversized_module_input");
        let module_dir = root.join("demo");
        write_native_input_fixture(&module_dir);
        write_local_manifest(&module_dir);
        let path = module_dir.join("oversized.vo");
        fs::File::create(&path)
            .unwrap()
            .set_len(u64::try_from(NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES).unwrap() + 1)
            .unwrap();

        let error = test_native_extension_input_fingerprint(
            &module_dir,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap_err();
        assert_eq!(error.kind, ModuleSystemErrorKind::ReadFailed);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_build_input_walk_enforces_depth_and_entry_budgets() {
        let root = temp_dir("vo_native_walk_budget");
        let rust_dir = root.join("rust");
        fs::create_dir_all(&rust_dir).unwrap();
        let mut deep = rust_dir.clone();
        for _ in 0..=NATIVE_EXTENSION_MAX_INPUT_DIRECTORY_DEPTH {
            deep.push("d");
            fs::create_dir(&deep).unwrap();
        }
        let depth_error = collect_native_build_inputs(&rust_dir).unwrap_err();
        assert_eq!(depth_error.kind, ModuleSystemErrorKind::ReadFailed);
        fs::remove_dir_all(&rust_dir).unwrap();

        fs::create_dir_all(&rust_dir).unwrap();
        fs::write(rust_dir.join("a"), b"a").unwrap();
        fs::write(rust_dir.join("b"), b"b").unwrap();
        let mut walk = NativeInputWalkState::with_entry_limit(1);
        let entry_error =
            collect_native_build_input_paths(&rust_dir, &rust_dir, &mut walk, 0, &mut Vec::new())
                .unwrap_err();
        assert_eq!(entry_error.kind, ModuleSystemErrorKind::ReadFailed);

        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn native_build_input_walk_rejects_symbolic_links() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("vo_native_walk_symlink");
        let rust_dir = root.join("rust");
        fs::create_dir_all(&rust_dir).unwrap();
        fs::write(rust_dir.join("source.rs"), b"source").unwrap();
        symlink(rust_dir.join("source.rs"), rust_dir.join("alias.rs")).unwrap();

        let error = collect_native_build_inputs(&rust_dir).unwrap_err();
        assert_eq!(error.kind, ModuleSystemErrorKind::ReadFailed);

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn local_patch_input_walk_enforces_depth_and_shared_entry_budget() {
        let root = temp_dir("vo_patch_walk_budget");
        let first = root.join("first");
        let second = root.join("second");
        fs::create_dir_all(&first).unwrap();
        fs::create_dir_all(&second).unwrap();
        fs::write(first.join("a"), b"a").unwrap();
        fs::write(second.join("b"), b"b").unwrap();
        let mut files = BTreeSet::new();
        let mut walk = NativeInputWalkState::with_entry_limit(1);
        collect_local_patch_input_files(&first, &mut files, &mut walk).unwrap();
        assert!(collect_local_patch_input_files(&second, &mut files, &mut walk).is_err());

        let deep_root = root.join("deep");
        fs::create_dir_all(&deep_root).unwrap();
        let mut deep = deep_root.clone();
        for _ in 0..=NATIVE_EXTENSION_MAX_INPUT_DIRECTORY_DEPTH {
            deep.push("d");
            fs::create_dir(&deep).unwrap();
        }
        let mut files = BTreeSet::new();
        let mut walk = NativeInputWalkState::default();
        assert!(collect_local_patch_input_files(&deep_root, &mut files, &mut walk).is_err());

        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn local_patch_input_walk_rejects_symbolic_links() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("vo_patch_walk_symlink");
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join("source.rs"), b"source").unwrap();
        symlink(root.join("source.rs"), root.join("alias.rs")).unwrap();
        let mut files = BTreeSet::new();
        let mut walk = NativeInputWalkState::default();

        assert!(collect_local_patch_input_files(&root, &mut files, &mut walk).is_err());

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_input_marker_rejects_corruption_and_a_b_a_transitions() {
        let root = temp_dir("vo_native_input_marker");
        let module_dir = root.join("demo");
        write_native_input_fixture(&module_dir);
        write_local_manifest(&module_dir);
        let native_path = local_manifest(&module_dir)
            .resolve_local_native_path(&module_dir)
            .unwrap()
            .unwrap();
        fs::create_dir_all(native_path.parent().unwrap()).unwrap();
        fs::write(&native_path, b"artifact").unwrap();
        write_native_extension_abi_marker(&native_path).unwrap();

        let source_path = module_dir.join("rust/src/lib.rs");
        let source_a = fs::read(&source_path).unwrap();
        let fingerprint_a = native_extension_input_fingerprint(&module_dir).unwrap();
        write_native_extension_input_marker(&native_path, &fingerprint_a).unwrap();
        assert!(native_extension_cache_is_valid(&native_path, &module_dir).unwrap());

        fs::write(&source_path, b"pub fn value() -> u8 { 2 }\n").unwrap();
        let fingerprint_b = native_extension_input_fingerprint(&module_dir).unwrap();
        assert_ne!(fingerprint_a, fingerprint_b);
        write_native_extension_input_marker(&native_path, &fingerprint_b).unwrap();

        fs::write(&source_path, source_a).unwrap();
        assert_eq!(
            native_extension_input_fingerprint(&module_dir).unwrap(),
            fingerprint_a,
        );
        assert!(!native_extension_cache_is_valid(&native_path, &module_dir).unwrap());

        fs::write(
            native_extension_input_marker_path(&native_path),
            b"truncated",
        )
        .unwrap();
        assert!(!native_extension_cache_is_valid(&native_path, &module_dir).unwrap());

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_abi_marker_binds_artifact_content() {
        let root = temp_dir("vo_native_abi_marker");
        fs::create_dir_all(&root).unwrap();
        let native_path = root.join(current_platform_library_name("libdemo"));
        fs::write(&native_path, b"artifact-a").unwrap();
        write_native_extension_abi_marker(&native_path).unwrap();
        assert!(native_extension_build_marker_matches(&native_path));

        fs::write(&native_path, b"artifact-b").unwrap();
        assert!(!native_extension_build_marker_matches(&native_path));

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn cached_local_extension_checks_original_inputs_and_load_copy() {
        let root = temp_dir("vo_native_cached_spec");
        let module_dir = root.join("demo");
        let native_path = write_abi_native_extension_fixture(&module_dir);
        let workspace_discovery = vo_module::workspace::WorkspaceDiscovery::Disabled;
        let load_path = ensure_local_native_extension_built_with_workspace(
            &module_dir,
            &native_path,
            &workspace_discovery,
        )
        .unwrap();
        let spec = NativeExtensionSpec::new(
            "demo",
            "github.com/example/demo",
            load_path.clone(),
            module_dir.join("vo.mod"),
        );

        let refreshed = ensure_local_native_extension_built_with_workspace(
            &module_dir,
            &native_path,
            &workspace_discovery,
        )
        .unwrap();
        assert!(
            paths_refer_to_same_file(&load_path, &refreshed),
            "{} != {}",
            load_path.display(),
            refreshed.display(),
        );
        assert!(is_local_native_extension_load_copy(&spec.native_path));
        assert!(native_extension_build_marker_matches(&spec.native_path));
        let manifest_content = fs::read_to_string(&spec.manifest_path).unwrap();
        let parsed_mod = ModFile::parse(&manifest_content).unwrap();
        assert_eq!(
            parsed_mod.module.as_github().unwrap().as_str(),
            spec.module_owner,
        );
        let parsed_manifest =
            parse_ext_manifest_content(&manifest_content, &spec.manifest_path).unwrap();
        assert_eq!(parsed_manifest.name, spec.name);
        let native_bytes = read_bounded_file(
            &native_path,
            NATIVE_EXTENSION_MAX_ARTIFACT_BYTES,
            "test native extension",
        )
        .unwrap();
        let expected_load_path =
            native_extension_load_copy_path_for_bytes(&native_path, &native_bytes);
        assert!(
            paths_refer_to_same_file(&expected_load_path, &spec.native_path),
            "expected {} but cached {}",
            expected_load_path.display(),
            spec.native_path.display(),
        );
        assert_eq!(
            read_bounded_text_file(
                &native_extension_abi_marker_path(&spec.native_path),
                NATIVE_EXTENSION_MAX_MARKER_BYTES,
                "test ABI marker",
            )
            .unwrap(),
            native_extension_abi_marker_for_bytes(&native_bytes),
        );
        assert!(cached_native_extension_spec_is_current_with_workspace(
            &spec,
            &workspace_discovery,
        ));
        fs::write(
            module_dir.join("rust/src/lib.rs"),
            abi_native_extension_source(2),
        )
        .unwrap();
        assert!(!cached_native_extension_spec_is_current_with_workspace(
            &spec,
            &workspace_discovery,
        ));

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn native_build_file_lock_serializes_concurrent_builders_for_one_artifact() {
        const WORKERS: usize = 8;
        let root = temp_dir("vo_native_build_lock");
        let native_path = Arc::new(root.join(current_platform_library_name("libdemo")));
        let barrier = Arc::new(Barrier::new(WORKERS));
        let active = Arc::new(AtomicUsize::new(0));
        let max_active = Arc::new(AtomicUsize::new(0));
        let mut workers = Vec::new();

        for _ in 0..WORKERS {
            let native_path = Arc::clone(&native_path);
            let barrier = Arc::clone(&barrier);
            let active = Arc::clone(&active);
            let max_active = Arc::clone(&max_active);
            let module_dir = root.clone();
            workers.push(std::thread::spawn(move || {
                barrier.wait();
                let _guard =
                    acquire_native_extension_build_file_lock(&module_dir, &native_path).unwrap();
                let now = active.fetch_add(1, AtomicOrdering::SeqCst) + 1;
                max_active.fetch_max(now, AtomicOrdering::SeqCst);
                std::thread::sleep(std::time::Duration::from_millis(5));
                active.fetch_sub(1, AtomicOrdering::SeqCst);
            }));
        }
        for worker in workers {
            worker.join().unwrap();
        }

        assert_eq!(max_active.load(AtomicOrdering::SeqCst), 1);
        fs::remove_dir_all(&root).unwrap();
    }
}
