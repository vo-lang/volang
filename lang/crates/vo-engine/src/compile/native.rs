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
    vfs::{normalize_fs_path, FileSystem, RealFs, MAX_TEXT_FILE_BYTES},
};
use vo_module::cache::validate::InstalledModuleError;
use vo_module::ext_manifest::{parse_ext_manifest_content, ExtensionManifest, NativeBuildManifest};
use vo_module::readiness::{
    check_materialized_modules_readiness, check_project_readiness, ReadinessFailure, ReadyModule,
};
use vo_module::schema::lockfile::LockedModule;
use vo_module::schema::modfile::ModFile;
use vo_runtime::ext_loader::{ExtensionLoader, NativeExtensionSpec, ABI_FINGERPRINT, ABI_VERSION};

use super::cache::collect_module_compile_input_files;
use super::host_input::{HostEntryIdentity, HostEntryKind, HostMetadataGeneration};
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
const NATIVE_EXTENSION_MAX_ARTIFACT_BYTES: usize = vo_module::MAX_MODULE_ARTIFACT_BYTES_USIZE;
const NATIVE_EXTENSION_MAX_MARKER_BYTES: usize = 4 * 1024;
const NATIVE_EXTENSION_MAX_CARGO_METADATA_BYTES: usize = 32 * 1024 * 1024;
const NATIVE_EXTENSION_MAX_CARGO_BUILD_OUTPUT_BYTES: usize = 64 * 1024 * 1024;
const NATIVE_EXTENSION_FFI_SOURCE_FINGERPRINT_ENV: &str = "VO_FFI_SOURCE_FINGERPRINT";
const NATIVE_EXTENSION_RUSTC_FINGERPRINT_CFG: &str = "vo_ffi_input_fingerprint";
static NATIVE_MARKER_TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);
static NATIVE_BUILD_LOCKS: OnceLock<Mutex<HashMap<PathBuf, Weak<Mutex<()>>>>> = OnceLock::new();
static NATIVE_LOAD_STORE_LOCKS: OnceLock<Mutex<HashMap<PathBuf, Weak<Mutex<()>>>>> =
    OnceLock::new();

#[derive(Debug)]
struct NativeLoadCopyLease {
    _file: fs::File,
}

#[derive(Debug)]
struct NativeLoadCopy {
    path: PathBuf,
    lease: Arc<NativeLoadCopyLease>,
}

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
/// validated `ProjectDeps` context. Workspace sources can remove registry
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
            vo_module::Error::MissingReleaseArtifact { detail, .. } => ModuleSystemError::new(
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
        None,
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
        None,
    )
}

pub(super) fn prepare_native_extension_specs_with_readiness_and_workspace(
    manifests: &[ExtensionManifest],
    ready_modules: &[ReadyModule],
    mod_root: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    frozen_input_fs: Option<&super::snapshot::ResolverFs>,
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
        let resolved = prepare_extension_spec(
            manifest,
            ready_modules,
            &mod_root,
            workspace_discovery,
            frozen_input_fs,
        )?;
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

fn native_extension_spec_with_load_copy(
    manifest: &ExtensionManifest,
    module_owner: &str,
    load_copy: NativeLoadCopy,
) -> NativeExtensionSpec {
    let mut spec = native_extension_spec(manifest, module_owner, load_copy.path);
    spec.retain_lifetime_resource(load_copy.lease);
    spec
}

fn local_extension_module_owner(manifest: &ExtensionManifest) -> Result<String, ModuleSystemError> {
    let module = manifest.module_owner.as_ref().ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::MissingMetadata,
            format!(
                "native extension metadata from {} is missing the module owner retained from its parsed vo.mod generation",
                manifest.manifest_path.display()
            ),
        )
        .with_path(&manifest.manifest_path)
    })?;
    Ok(module.as_str().to_string())
}

fn validate_native_extension_manifest(
    manifest: &ExtensionManifest,
) -> Result<(), ModuleSystemError> {
    manifest.validate().map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            error.to_string(),
        )
        .with_path(&manifest.manifest_path)
    })
}

fn manifest_native_build(
    manifest: &ExtensionManifest,
) -> Result<&NativeBuildManifest, ModuleSystemError> {
    validate_native_extension_manifest(manifest)?;
    manifest.native_build().ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            format!(
                "local native extension requires [build.native] in {}",
                manifest.manifest_path.display(),
            ),
        )
        .with_path(&manifest.manifest_path)
    })
}

fn manifest_local_native_cargo_build<'a>(
    manifest: &'a ExtensionManifest,
    module_dir: &Path,
) -> Result<(PathBuf, Option<&'a str>), ModuleSystemError> {
    match manifest_native_build(manifest)? {
        NativeBuildManifest::Cargo { manifest, package } => {
            Ok((module_dir.join(manifest), package.as_deref()))
        }
        NativeBuildManifest::Prebuilt { .. } => Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ValidationFailed,
            "native extension build kind is prebuilt; a Cargo build was required",
        )
        .with_path(&manifest.manifest_path)),
    }
}

fn manifest_local_prebuilt_native_path(
    manifest: &ExtensionManifest,
    module_dir: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    manifest
        .resolve_prebuilt_native_path(module_dir)
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
                "native extension build kind is cargo; a prebuilt path was required",
            )
            .with_path(&manifest.manifest_path)
        })
}

#[cfg(test)]
fn configured_native_cargo_build(
    module_dir: &Path,
) -> Result<(PathBuf, Option<String>), ModuleSystemError> {
    let manifest_path = module_dir.join("vo.mod");
    let content = read_bounded_text_file(
        &manifest_path,
        MAX_TEXT_FILE_BYTES,
        "native extension manifest",
    )
    .map_err(|error| native_build_input_read_error(&manifest_path, error))?;
    let manifest = parse_ext_manifest_content(&content, &manifest_path).map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ParseFailed,
            format!(
                "failed to parse native extension manifest at {}: {error}",
                manifest_path.display(),
            ),
        )
        .with_path(&manifest_path)
    })?;
    let (cargo_manifest, package) = manifest_local_native_cargo_build(&manifest, module_dir)?;
    Ok((cargo_manifest, package.map(str::to_string)))
}

fn canonical_native_cargo_manifest(
    module_dir: &Path,
    configured_manifest: &Path,
) -> Result<(PathBuf, PathBuf), ModuleSystemError> {
    let module_root = module_dir
        .canonicalize()
        .map_err(|error| native_build_input_read_error(module_dir, error))?;
    let relative_manifest = configured_manifest
        .strip_prefix(module_dir)
        .or_else(|_| configured_manifest.strip_prefix(&module_root))
        .map_err(|_| {
            native_build_input_read_error(
                configured_manifest,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension Cargo manifest must be inside the module root",
                ),
            )
        })?;
    let components = relative_manifest.components().collect::<Vec<_>>();
    if components.is_empty() {
        return Err(native_build_input_read_error(
            configured_manifest,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "native extension Cargo manifest path is empty",
            ),
        ));
    }
    let mut current = module_root.clone();
    for (index, component) in components.iter().enumerate() {
        let std::path::Component::Normal(component) = component else {
            return Err(native_build_input_read_error(
                configured_manifest,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension Cargo manifest path must be normalized",
                ),
            ));
        };
        current.push(component);
        let metadata = fs::symlink_metadata(&current)
            .map_err(|error| native_build_input_read_error(&current, error))?;
        if metadata.file_type().is_symlink() {
            return Err(native_build_input_read_error(
                &current,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension Cargo manifest path must not contain symbolic links",
                ),
            ));
        }
        let is_manifest = index + 1 == components.len();
        let valid_type = if is_manifest {
            metadata.file_type().is_file()
        } else {
            metadata.file_type().is_dir()
        };
        if !valid_type {
            let expected = if is_manifest {
                "a regular file"
            } else {
                "a real directory"
            };
            return Err(native_build_input_read_error(
                &current,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("native extension Cargo manifest path component must be {expected}"),
                ),
            ));
        }
    }
    let manifest_path = canonical_regular_native_input(
        configured_manifest,
        MAX_TEXT_FILE_BYTES,
        "native extension Cargo manifest",
    )?;
    if !manifest_path.starts_with(&module_root) {
        return Err(native_build_input_read_error(
            configured_manifest,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "native extension Cargo manifest resolves outside module root {}",
                    module_root.display(),
                ),
            ),
        ));
    }
    let manifest_dir = manifest_path
        .parent()
        .ok_or_else(|| {
            native_build_input_read_error(
                &manifest_path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension Cargo manifest has no parent",
                ),
            )
        })?
        .to_path_buf();
    Ok((manifest_path, manifest_dir))
}

fn canonical_prebuilt_native_path(
    module_dir: &Path,
    configured_path: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    let module_root = module_dir
        .canonicalize()
        .map_err(|error| native_build_input_read_error(module_dir, error))?;
    let relative_path = configured_path
        .strip_prefix(module_dir)
        .or_else(|_| configured_path.strip_prefix(&module_root))
        .map_err(|_| {
            native_build_input_read_error(
                configured_path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "prebuilt native extension path must be inside the module root",
                ),
            )
        })?;
    let components = relative_path.components().collect::<Vec<_>>();
    if components.is_empty() {
        return Err(native_build_input_read_error(
            configured_path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "prebuilt native extension path is empty",
            ),
        ));
    }
    let mut current = module_root.clone();
    for (index, component) in components.iter().enumerate() {
        let std::path::Component::Normal(component) = component else {
            return Err(native_build_input_read_error(
                configured_path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "prebuilt native extension path must be normalized",
                ),
            ));
        };
        current.push(component);
        let metadata = fs::symlink_metadata(&current)
            .map_err(|error| native_build_input_read_error(&current, error))?;
        if metadata.file_type().is_symlink() {
            return Err(native_build_input_read_error(
                &current,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "prebuilt native extension path must not contain symbolic links",
                ),
            ));
        }
        let is_artifact = index + 1 == components.len();
        if (is_artifact && !metadata.file_type().is_file())
            || (!is_artifact && !metadata.file_type().is_dir())
        {
            return Err(native_build_input_read_error(
                &current,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    if is_artifact {
                        "prebuilt native extension must be a regular file"
                    } else {
                        "prebuilt native extension parent must be a real directory"
                    },
                ),
            ));
        }
    }
    let artifact = canonical_regular_native_input(
        configured_path,
        NATIVE_EXTENSION_MAX_ARTIFACT_BYTES,
        "prebuilt native extension",
    )?;
    if !artifact.starts_with(&module_root) {
        return Err(native_build_input_read_error(
            configured_path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "prebuilt native extension resolves outside the module root",
            ),
        ));
    }
    if !is_platform_native_library(&artifact) {
        return Err(native_build_input_read_error(
            &artifact,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "prebuilt native extension has the wrong library suffix for this host",
            ),
        ));
    }
    Ok(artifact)
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
    frozen_input_fs: Option<&super::snapshot::ResolverFs>,
) -> Result<NativeExtensionSpec, ModuleSystemError> {
    validate_native_extension_manifest(manifest)?;
    if manifest
        .declared_native_target(current_target_triple())
        .is_none()
    {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::Missing,
            format!(
                "vo.mod does not declare extension-native support for target {}",
                current_target_triple(),
            ),
        )
        .with_path(&manifest.manifest_path));
    }
    let module_dir = extension_manifest_module_dir(manifest)?;
    if !module_dir.starts_with(mod_root) {
        let module_owner = local_extension_module_owner(manifest)?;
        let load_copy = prepare_local_native_extension_load_path(
            manifest,
            &module_dir,
            &module_owner,
            mod_root,
            workspace_discovery,
            frozen_input_fs,
        )?;
        return Ok(native_extension_spec_with_load_copy(
            manifest,
            &module_owner,
            load_copy,
        ));
    }

    let ready = ready_module_for_cached_extension(&module_dir, mod_root, ready_modules)?;
    prepare_cached_extension_spec(manifest, ready, &module_dir)
}

fn prepare_local_native_extension_load_path(
    manifest: &ExtensionManifest,
    module_dir: &Path,
    module_owner: &str,
    mod_root: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    _frozen_input_fs: Option<&super::snapshot::ResolverFs>,
) -> Result<NativeLoadCopy, ModuleSystemError> {
    match manifest_native_build(manifest)? {
        NativeBuildManifest::Cargo { .. } => ensure_local_native_extension_built_with_workspace(
            manifest,
            module_dir,
            mod_root,
            workspace_discovery,
        ),
        NativeBuildManifest::Prebuilt { .. } => {
            let configured = manifest_local_prebuilt_native_path(manifest, module_dir)?;
            // Prebuilt bytes are native-only input. Resolve and freeze them
            // here, after analysis has reached this extension, just like a
            // Cargo adapter's complete input generation.
            let native_path = canonical_prebuilt_native_path(module_dir, &configured)?;
            let load_copy = materialize_native_extension_load_copy(&native_path)?;
            validate_built_native_extension_abi(&load_copy.path, module_owner)?;
            Ok(load_copy)
        }
    }
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
    workspace_members: Vec<String>,
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
    #[serde(default)]
    dependencies: Vec<CargoMetadataDeclaredDependency>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadataDeclaredDependency {
    name: String,
    rename: Option<String>,
    kind: Option<String>,
    target: Option<String>,
    path: Option<PathBuf>,
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
    name: String,
    pkg: String,
    #[serde(default)]
    dep_kinds: Vec<CargoMetadataDependencyKind>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadataDependencyKind {
    kind: Option<String>,
    target: Option<String>,
}

fn cargo_metadata_dependency_matches_declaration(
    resolved: &CargoMetadataDependency,
    declared: &CargoMetadataDeclaredDependency,
) -> bool {
    let declared_name = declared.rename.as_deref().unwrap_or(&declared.name);
    declared_name == resolved.name
        && (resolved.dep_kinds.is_empty()
            || resolved
                .dep_kinds
                .iter()
                .any(|kind| kind.kind == declared.kind && kind.target == declared.target))
}

#[derive(Debug, Clone)]
struct NativeCargoContext {
    manifest_dir: PathBuf,
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
    configured_manifest: &Path,
    configured_package: Option<&str>,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<NativeCargoContext, ModuleSystemError> {
    let (manifest_path, manifest_dir) =
        canonical_native_cargo_manifest(module_dir, configured_manifest)?;
    let module_dir = module_dir
        .canonicalize()
        .map_err(|error| native_build_input_read_error(module_dir, error))?;
    let located_workspace_root =
        locate_native_cargo_workspace_root(&manifest_dir, &manifest_path, workspace_discovery)?;
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
    let patch_configs = if native_cargo_uses_volang_git_source(
        &manifest_path,
        &manifest_dir,
        &located_workspace_root,
        &lock_bytes,
    )? {
        local_volang_cargo_patch_configs(&lock_bytes, &located_lock_path)?
    } else {
        Vec::new()
    };
    let mut command = std::process::Command::new("cargo");
    command
        .arg("metadata")
        .args(["--format-version", "1"])
        .arg("--locked")
        .arg("--filter-platform")
        .arg(current_target_triple())
        .arg("--manifest-path")
        .arg(&manifest_path);
    for config in &patch_configs {
        command.arg("--config").arg(config);
    }
    command
        .current_dir(&manifest_dir)
        .env_remove("CARGO_BUILD_TARGET");
    apply_workspace_discovery_to_command(&mut command, workspace_discovery);
    let output =
        run_command_with_bounded_output(&mut command, NATIVE_EXTENSION_MAX_CARGO_METADATA_BYTES)
            .map_err(|error| {
                native_cargo_command_start_error(
                    "metadata",
                    &manifest_path,
                    ModuleSystemErrorKind::BuildFailed,
                    error,
                )
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
                manifest_dir.display(),
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

    let workspace_members = metadata
        .workspace_members
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let mut root_packages = metadata
        .packages
        .iter()
        .filter(|package| match configured_package {
            Some(name) => package.name == name && workspace_members.contains(package.id.as_str()),
            None => package
                .manifest_path
                .canonicalize()
                .is_ok_and(|path| path == manifest_path),
        })
        .collect::<Vec<_>>();
    if root_packages.len() != 1 {
        let selector = configured_package.map_or_else(
            || manifest_path.display().to_string(),
            |package| format!("package {package:?}"),
        );
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "cargo metadata identified {} native extension packages for {selector}; expected exactly one",
                    root_packages.len(),
                ),
            )
            .with_path(&manifest_path)
        );
    }
    let root_package = root_packages
        .pop()
        .expect("exactly one native extension package was checked");
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
    let mut local_package_roots_by_id = BTreeMap::new();
    for package in &metadata.packages {
        if package.source.is_some() || !reachable.contains(&package.id) {
            continue;
        }
        let package_manifest = canonical_regular_native_input(
            &package.manifest_path,
            MAX_TEXT_FILE_BYTES,
            "local Cargo package manifest",
        )?;
        let package_root = package_manifest
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();
        local_package_roots.insert(package_root.clone());
        local_package_roots_by_id.insert(package.id.as_str(), package_root);
    }
    if local_package_roots.len() > NATIVE_EXTENSION_MAX_INPUT_FILES {
        return Err(native_module_input_file_limit_error(&manifest_dir));
    }
    for package in &metadata.packages {
        let Some(package_root) = local_package_roots_by_id.get(package.id.as_str()) else {
            continue;
        };
        let node = nodes.get(package.id.as_str()).ok_or_else(|| {
            native_build_input_read_error(
                &package.manifest_path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "Cargo metadata omitted the resolved node for a captured local package",
                ),
            )
        })?;
        for resolved_dependency in &node.deps {
            let Some(expected_root) =
                local_package_roots_by_id.get(resolved_dependency.pkg.as_str())
            else {
                continue;
            };
            for declared_dependency in package.dependencies.iter().filter(|declared| {
                declared.path.is_some()
                    && cargo_metadata_dependency_matches_declaration(resolved_dependency, declared)
            }) {
                let declared_path = declared_dependency
                    .path
                    .as_deref()
                    .expect("Cargo path dependency filter requires a path");
                let declared_path = if declared_path.is_absolute() {
                    declared_path.to_path_buf()
                } else {
                    package_root.join(declared_path)
                };
                let declared_root = canonical_directory_native_input(
                    &declared_path,
                    "local Cargo path dependency",
                )?;
                if &declared_root != expected_root {
                    return Err(native_build_input_read_error(
                        &declared_path,
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!(
                                "resolved Cargo dependency {} points at {}, while its declared path resolves to {}",
                                resolved_dependency.name,
                                expected_root.display(),
                                declared_root.display(),
                            ),
                        ),
                    ));
                }
            }
        }
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
    let target_directory = normalize_native_output_path(&metadata.target_directory)?;
    validate_native_cargo_target_directory_layout(
        &target_directory,
        &module_dir,
        &manifest_dir,
        &workspace_root,
        &local_package_roots,
    )?;
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
        manifest_dir,
        manifest_path,
        workspace_root,
        lock_path,
        target_directory,
        root_package_id: root_package.id.clone(),
        root_package_name: root_package.name.clone(),
        root_target_name,
        local_package_roots: local_package_roots.into_iter().collect(),
        patch_configs,
        metadata_digest,
    })
}

fn canonical_regular_native_input(
    path: &Path,
    max_bytes: usize,
    description: &str,
) -> Result<PathBuf, ModuleSystemError> {
    let opened = super::host_input::read_stable_regular_file_snapshot(path, max_bytes)
        .map_err(|error| native_build_input_read_error(path, error))?;
    let canonical = path
        .canonicalize()
        .map_err(|error| native_build_input_read_error(path, error))?;
    let canonical_opened =
        super::host_input::read_stable_regular_file_snapshot(&canonical, max_bytes)
            .map_err(|error| native_build_input_read_error(&canonical, error))?;
    if opened.generation != canonical_opened.generation || opened.bytes != canonical_opened.bytes {
        return Err(native_build_input_read_error(
            path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "{description} changed identity or bytes while its canonical path was established"
                ),
            ),
        ));
    }
    Ok(canonical)
}

fn canonical_directory_native_input(
    path: &Path,
    description: &str,
) -> Result<PathBuf, ModuleSystemError> {
    let opened = super::host_input::validate_stable_directory_path(path)
        .map_err(|error| native_build_input_read_error(path, error))?;
    let canonical = path
        .canonicalize()
        .map_err(|error| native_build_input_read_error(path, error))?;
    let canonical_opened = super::host_input::validate_stable_directory_path(&canonical)
        .map_err(|error| native_build_input_read_error(&canonical, error))?;
    if opened != canonical_opened {
        return Err(native_build_input_read_error(
            path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("{description} changed identity while its canonical path was established"),
            ),
        ));
    }
    Ok(canonical)
}

fn validate_native_cargo_target_directory_layout(
    target_directory: &Path,
    module_dir: &Path,
    manifest_dir: &Path,
    workspace_root: &Path,
    local_package_roots: &BTreeSet<PathBuf>,
) -> Result<(), ModuleSystemError> {
    let overlapping_source = [module_dir, manifest_dir, workspace_root]
        .into_iter()
        .find(|root| root.starts_with(target_directory))
        .or_else(|| {
            local_package_roots
                .iter()
                .find(|root| root.starts_with(target_directory))
                .map(PathBuf::as_path)
        });
    let Some(overlapping_source) = overlapping_source else {
        return Ok(());
    };
    Err(ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::ValidationFailed,
        format!(
            "Cargo target directory {} contains native extension source root {}; configure a dedicated generated-output subdirectory",
            target_directory.display(),
            overlapping_source.display(),
        ),
    )
    .with_path(target_directory))
}

fn locate_native_cargo_workspace_root(
    manifest_dir: &Path,
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
        .current_dir(manifest_dir);
    apply_workspace_discovery_to_command(&mut command, workspace_discovery);
    let output =
        run_command_with_bounded_output(&mut command, MAX_TEXT_FILE_BYTES).map_err(|error| {
            native_cargo_command_start_error(
                "locate-project",
                manifest_path,
                ModuleSystemErrorKind::ReadFailed,
                error,
            )
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
    manifest_path: &Path,
    manifest_dir: &Path,
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
    insert_existing_context_file(&mut manifests, manifest_path.to_path_buf())?;
    if manifest_dir.starts_with(workspace_root) {
        for ancestor in manifest_dir.ancestors() {
            if !ancestor.starts_with(workspace_root) {
                break;
            }
            insert_existing_context_file(&mut manifests, ancestor.join("Cargo.toml"))?;
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

fn ensure_local_native_extension_built_with_workspace(
    manifest: &ExtensionManifest,
    module_dir: &Path,
    mod_root: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<NativeLoadCopy, ModuleSystemError> {
    let module_owner = local_extension_module_owner(manifest)?;
    let module_dir = module_dir
        .canonicalize()
        .map_err(|error| native_build_input_read_error(module_dir, error))?;
    let (cargo_manifest, package) = manifest_local_native_cargo_build(manifest, &module_dir)?;
    if !cargo_manifest
        .try_exists()
        .map_err(|error| native_build_input_read_error(&cargo_manifest, error))?
    {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::Missing,
            format!(
                "native extension Cargo manifest is missing at {}",
                cargo_manifest.display()
            ),
        )
        .with_path(&cargo_manifest));
    }
    let workspace_discovery = normalized_workspace_discovery(&module_dir, workspace_discovery)?;
    let coordinator_identity = native_extension_build_coordinator_identity(
        manifest,
        &module_dir,
        &cargo_manifest,
        &module_owner,
    )?;
    let _coordinator =
        vo_module::cache::acquire_native_build_coordinator(mod_root, &coordinator_identity)
            .map_err(|error| native_extension_build_coordinator_error(mod_root, error))?;

    for attempt in 0..NATIVE_EXTENSION_BUILD_ATTEMPTS {
        let inspected_cargo = inspect_native_cargo_context(
            &module_dir,
            &cargo_manifest,
            package,
            &workspace_discovery,
        )?;
        let build_lock_path = native_extension_build_file_lock_path(&inspected_cargo);
        let build_lock = native_extension_build_lock(&build_lock_path);
        let _build_guard = build_lock
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _cross_process_build_guard =
            acquire_native_extension_build_file_lock(&build_lock_path)?;

        let input_state = capture_native_extension_input_state(
            &module_dir,
            &cargo_manifest,
            package,
            &workspace_discovery,
        )?;
        if native_extension_build_file_lock_path(&input_state.cargo) != build_lock_path {
            emit_native_extension_inputs_changed(&module_dir);
            if attempt + 1 < NATIVE_EXTENSION_BUILD_ATTEMPTS {
                continue;
            }
            return Err(native_extension_inputs_kept_changing_error(&module_dir));
        }
        let artifact = build_native_extension(&input_state, &workspace_discovery)?;
        if artifact.fresh
            && native_extension_cache_is_valid_for_fingerprint(
                &artifact.path,
                &input_state.build_token,
            )?
        {
            emit_compile_log(
                CompileLogRecord::new("vo-engine", "native_extension_cached")
                    .path(artifact.path.display().to_string()),
            );
        }

        let post_build_state = capture_native_extension_input_state(
            &module_dir,
            &cargo_manifest,
            package,
            &workspace_discovery,
        )?;
        if input_state.fingerprint != post_build_state.fingerprint
            || input_state.generation != post_build_state.generation
            || input_state.build_token != post_build_state.build_token
            || input_state.cargo.metadata_digest != post_build_state.cargo.metadata_digest
            || native_extension_build_file_lock_path(&post_build_state.cargo) != build_lock_path
        {
            emit_native_extension_inputs_changed(&module_dir);
            if attempt + 1 < NATIVE_EXTENSION_BUILD_ATTEMPTS {
                continue;
            }
            return Err(native_extension_inputs_kept_changing_error(&module_dir));
        }

        let load_copy = materialize_native_extension_load_copy(&artifact.path)?;
        validate_built_native_extension_abi(&load_copy.path, &module_owner)?;
        write_native_extension_abi_marker(&artifact.path)?;
        write_native_extension_input_marker(&artifact.path, &post_build_state.build_token)?;
        let current_load_copy = materialize_native_extension_load_copy(&artifact.path)?;
        let final_state = capture_native_extension_input_state(
            &module_dir,
            &cargo_manifest,
            package,
            &workspace_discovery,
        )?;
        if paths_refer_to_same_file(&load_copy.path, &current_load_copy.path)
            && final_state.fingerprint == post_build_state.fingerprint
            && final_state.generation == post_build_state.generation
            && final_state.build_token == post_build_state.build_token
            && final_state.cargo.metadata_digest == post_build_state.cargo.metadata_digest
            && native_extension_build_file_lock_path(&final_state.cargo) == build_lock_path
        {
            return Ok(load_copy);
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

fn native_extension_build_coordinator_identity(
    manifest: &ExtensionManifest,
    module_dir: &Path,
    cargo_manifest: &Path,
    module_owner: &str,
) -> Result<String, ModuleSystemError> {
    let extension_manifest = manifest
        .manifest_path
        .canonicalize()
        .map_err(|error| native_build_input_read_error(&manifest.manifest_path, error))?;
    let cargo_manifest = cargo_manifest
        .canonicalize()
        .map_err(|error| native_build_input_read_error(cargo_manifest, error))?;
    let mut hasher = StableHasher::new("vo-native-extension-stable-build-coordinator-v1");
    hasher.update_path("module_dir", module_dir);
    hasher.update_path("extension_manifest", &extension_manifest);
    hasher.update_path("cargo_manifest", &cargo_manifest);
    hasher.update_str("module_owner", module_owner);
    hasher.update_str("host_target", current_target_triple());
    hasher.update_str(
        "profile",
        if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        },
    );
    // Mutable Cargo context deliberately stays out of this identity. The
    // coordinator must remain stable while target/config metadata drifts.
    Ok(format!("native-build:{}", hasher.finish()))
}

fn native_extension_build_coordinator_error(
    mod_root: &Path,
    error: vo_module::Error,
) -> ModuleSystemError {
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::BuildFailed,
        format!(
            "failed to acquire the native extension build coordinator in {}: {}",
            mod_root.display(),
            error,
        ),
    )
    .with_path(mod_root)
}

fn native_extension_build_lock(lock_path: &Path) -> Arc<Mutex<()>> {
    let key = lock_path
        .parent()
        .and_then(|parent| parent.canonicalize().ok())
        .map(|parent| {
            lock_path
                .file_name()
                .map_or(parent.clone(), |name| parent.join(name))
        })
        .unwrap_or_else(|| lock_path.to_path_buf());
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
    lock_path: &Path,
) -> Result<fs::File, ModuleSystemError> {
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
        .open(lock_path)
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
            .with_path(lock_path)
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
        .with_path(lock_path)
    })?;
    Ok(file)
}

fn native_extension_build_file_lock_path(cargo: &NativeCargoContext) -> PathBuf {
    let mut hasher = StableHasher::new("vo-native-extension-build-lock-v1");
    hasher.update_path("cargo_manifest", &cargo.manifest_path);
    hasher.update_str("cargo_package", &cargo.root_package_id);
    hasher.update_str("cargo_target", &cargo.root_target_name);
    hasher.update_str("host_target", current_target_triple());
    hasher.update_str(
        "profile",
        if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        },
    );
    cargo
        .target_directory
        .join(format!(".vo-build-lock-{}", hasher.finish_suffix()))
}

#[cfg(test)]
fn native_extension_input_fingerprint(module_dir: &Path) -> Result<String, ModuleSystemError> {
    let (cargo_manifest, package) = configured_native_cargo_build(module_dir)?;
    let workspace_discovery = normalized_workspace_discovery(
        module_dir,
        &vo_module::workspace::workspace_discovery_from_environment(),
    )?;
    let cargo = inspect_native_cargo_context(
        module_dir,
        &cargo_manifest,
        package.as_deref(),
        &workspace_discovery,
    )?;
    native_extension_input_fingerprint_with_workspace(
        module_dir,
        &cargo.manifest_path,
        &cargo.target_directory,
        &workspace_discovery,
    )
}

fn native_extension_input_fingerprint_with_workspace(
    module_dir: &Path,
    cargo_manifest: &Path,
    target_directory: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<String, ModuleSystemError> {
    compute_native_extension_input_fingerprint(
        module_dir,
        cargo_manifest,
        target_directory,
        workspace_discovery,
        || native_build_context_fingerprint_for_manifest(module_dir, cargo_manifest),
    )
}

fn compute_native_extension_input_fingerprint<F>(
    module_dir: &Path,
    cargo_manifest: &Path,
    target_directory: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    build_context_fingerprint: F,
) -> Result<String, ModuleSystemError>
where
    F: FnOnce() -> Result<String, ModuleSystemError>,
{
    let manifest_dir = cargo_manifest
        .parent()
        .ok_or_else(|| {
            native_build_input_read_error(
                cargo_manifest,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension Cargo manifest has no parent",
                ),
            )
        })?
        .to_path_buf();
    let manifest_dir = manifest_dir
        .canonicalize()
        .unwrap_or_else(|_| manifest_dir.to_path_buf());
    let native_build_inputs = collect_native_build_inputs(&manifest_dir, target_directory)?;
    let context_fingerprint = build_context_fingerprint()?;
    let mut hasher = native_build_input_hasher(
        cargo_manifest,
        &manifest_dir,
        workspace_discovery,
        &context_fingerprint,
    );
    hash_native_input_bytes(
        &mut hasher,
        "native_input_path",
        "native_input_bytes",
        &native_build_inputs,
    );
    drop(native_build_inputs);
    let module_compile_inputs =
        collect_native_module_compile_inputs(module_dir, workspace_discovery)?;
    hash_native_input_bytes(
        &mut hasher,
        "module_input_path",
        "module_input_bytes",
        &module_compile_inputs,
    );
    Ok(hasher.finish())
}

#[cfg(test)]
fn hash_native_build_inputs(
    cargo_manifest: &Path,
    manifest_dir: &Path,
    native_build_inputs: &[(PathBuf, Vec<u8>)],
    module_compile_inputs: &[(PathBuf, Vec<u8>)],
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    context_fingerprint: &str,
) -> String {
    let mut hasher = native_build_input_hasher(
        cargo_manifest,
        manifest_dir,
        workspace_discovery,
        context_fingerprint,
    );
    hash_native_input_bytes(
        &mut hasher,
        "native_input_path",
        "native_input_bytes",
        native_build_inputs,
    );
    hash_native_input_bytes(
        &mut hasher,
        "module_input_path",
        "module_input_bytes",
        module_compile_inputs,
    );
    hasher.finish()
}

fn native_build_input_hasher(
    cargo_manifest: &Path,
    manifest_dir: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    context_fingerprint: &str,
) -> StableHasher {
    let mut hasher = StableHasher::new("vo-native-extension-inputs-v3");
    hasher.update_path("cargo_manifest", cargo_manifest);
    hasher.update_path("manifest_dir", manifest_dir);
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
    hasher
}

fn hash_native_input_bytes(
    hasher: &mut StableHasher,
    path_label: &str,
    bytes_label: &str,
    inputs: &[(PathBuf, Vec<u8>)],
) {
    let mut sorted = inputs.iter().collect::<Vec<_>>();
    sorted.sort_by(|left, right| left.0.cmp(&right.0));
    for (path, bytes) in sorted {
        hasher.update_path(path_label, path);
        hasher.update_bytes(bytes_label, bytes);
    }
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
    cargo_manifest: &Path,
    package: Option<&str>,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<NativeExtensionInputState, ModuleSystemError> {
    let mut previous = capture_native_extension_input_state_once(
        module_dir,
        cargo_manifest,
        package,
        workspace_discovery,
    )?;
    for _ in 0..NATIVE_EXTENSION_BUILD_ATTEMPTS {
        let current = capture_native_extension_input_state_once(
            module_dir,
            cargo_manifest,
            package,
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
    cargo_manifest: &Path,
    package: Option<&str>,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<NativeExtensionInputState, ModuleSystemError> {
    let cargo =
        inspect_native_cargo_context(module_dir, cargo_manifest, package, workspace_discovery)?;
    let opaque_native_root = opaque_native_root_from_manifest(module_dir, &cargo.manifest_path)?;
    let base_fingerprint = native_extension_input_fingerprint_with_workspace(
        module_dir,
        &cargo.manifest_path,
        &cargo.target_directory,
        workspace_discovery,
    )?;
    let cargo_lock_bytes = read_bounded_file(
        &cargo.lock_path,
        MAX_TEXT_FILE_BYTES,
        "Cargo workspace lockfile",
    )
    .map_err(|error| native_build_input_read_error(&cargo.lock_path, error))?;
    let NativeCargoInputs {
        files: cargo_input_files,
        directories: cargo_input_directories,
    } = collect_cargo_local_package_inputs(&cargo.local_package_roots, &cargo.target_directory)?;
    let cargo_context_inputs = read_native_build_context_inputs(
        native_cargo_context_files(&cargo)?,
        &cargo.workspace_root,
    )?;
    let (module_roots, workfile) = native_module_input_roots(module_dir, workspace_discovery)?;

    let mut hasher = StableHasher::new("vo-native-extension-production-inputs-v2");
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
    for (path, bytes) in &cargo_input_files {
        hasher.update_path("cargo_local_input_path", path);
        hasher.update_bytes("cargo_local_input_bytes", bytes);
    }
    for (path, bytes) in &cargo_context_inputs {
        hasher.update_path("cargo_context_path", path);
        hasher.update_bytes("cargo_context_bytes", bytes);
    }
    let fingerprint = hasher.finish();

    let generation_module_roots = module_roots.into_iter().collect::<Vec<_>>();
    let generation_cargo_roots = cargo.local_package_roots.clone();
    let mut generation_files = cargo_input_files
        .iter()
        .map(|(path, _)| path.clone())
        .collect::<Vec<_>>();
    generation_files.extend(cargo_context_inputs.iter().map(|(path, _)| path.clone()));
    generation_files.push(cargo.manifest_path.clone());
    generation_files.push(cargo.lock_path.clone());
    if let Some(workfile) = workfile {
        generation_files.push(workfile);
    }
    drop(cargo_input_files);
    drop(cargo_context_inputs);
    drop(cargo_lock_bytes);
    let generation = native_input_generation(
        generation_module_roots,
        generation_cargo_roots,
        generation_files,
        cargo_input_directories,
        &cargo.target_directory,
        &opaque_native_root,
    )?;
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

fn opaque_native_root_from_manifest(
    module_dir: &Path,
    manifest_path: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    let module_root = module_dir
        .canonicalize()
        .map_err(|error| native_build_input_read_error(module_dir, error))?;
    let relative = manifest_path.strip_prefix(&module_root).map_err(|_| {
        native_build_input_read_error(
            manifest_path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "native extension Cargo manifest must stay below its module root",
            ),
        )
    })?;
    let mut components = relative.components();
    let Some(std::path::Component::Normal(root)) = components.next() else {
        return Err(native_build_input_read_error(
            manifest_path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "native extension Cargo manifest must have a dedicated top-level native root",
            ),
        ));
    };
    if components.next().is_none() {
        return Err(native_build_input_read_error(
            manifest_path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "native extension Cargo manifest must be nested below its dedicated top-level native root",
            ),
        ));
    }
    Ok(module_root.join(root))
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
    target_directory: &Path,
) -> Result<NativeCargoInputs, ModuleSystemError> {
    let roots = minimal_non_overlapping_roots(roots.to_vec());
    let mut paths = Vec::new();
    let mut walk = NativeInputWalkState::default();
    for root in roots {
        collect_native_build_input_paths(
            &root,
            &mut walk,
            &mut paths,
            NativeInputTreeKind::CargoPackage,
            target_directory,
        )?;
    }
    paths.sort();
    paths.dedup();
    let mut total_bytes = 0usize;
    let mut inputs = Vec::with_capacity(paths.len());
    for path in paths {
        let bytes = walk.captured_files.remove(&path).ok_or_else(|| {
            native_build_input_read_error(
                &path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native Cargo input walk did not retain the opened file bytes",
                ),
            )
        })?;
        total_bytes = total_bytes.checked_add(bytes.len()).ok_or_else(|| {
            native_build_input_read_error(
                &path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native Cargo local package input size overflow",
                ),
            )
        })?;
        if total_bytes > NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES {
            return Err(native_build_input_read_error(
                &path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native Cargo local package inputs exceed the {}-byte limit",
                        NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES,
                    ),
                ),
            ));
        }
        inputs.push((path, bytes));
    }
    Ok(NativeCargoInputs {
        files: inputs,
        directories: walk.visited_dirs,
    })
}

fn native_input_generation(
    module_roots: Vec<PathBuf>,
    cargo_roots: Vec<PathBuf>,
    files: Vec<PathBuf>,
    known_cargo_dirs: BTreeSet<PathBuf>,
    target_directory: &Path,
    opaque_native_root: &Path,
) -> Result<String, ModuleSystemError> {
    let canonical_roots = |roots: Vec<PathBuf>| {
        minimal_non_overlapping_roots(
            roots
                .into_iter()
                .map(|root| root.canonicalize().unwrap_or(root))
                .collect(),
        )
    };
    let module_roots = canonical_roots(module_roots);
    let cargo_roots = canonical_roots(cargo_roots);
    let mut hasher = StableHasher::new("vo-native-extension-input-generation-v4");
    hash_native_input_namespace_generations(&mut hasher, &module_roots, &cargo_roots, &files)?;

    let mut module_walk = NativeInputWalkState::default();
    let mut module_files = Vec::new();
    for root in module_roots {
        collect_native_build_input_paths_with_opaque_root(
            &root,
            &mut module_walk,
            &mut module_files,
            NativeInputTreeKind::ModuleTree,
            target_directory,
            Some(opaque_native_root),
        )?;
    }

    let cargo_entry_limit = NATIVE_EXTENSION_MAX_INPUT_ENTRIES.saturating_sub(module_walk.entries);
    let module_generation_entries = module_walk
        .directory_entries
        .len()
        .saturating_add(module_walk.captured_files.len());
    hash_native_input_walk(&mut hasher, "module", &module_walk)?;
    drop(module_walk);
    drop(module_files);

    let mut cargo_walk = NativeInputWalkState::with_entry_limit(cargo_entry_limit);
    let mut cargo_files = Vec::new();
    for root in cargo_roots {
        collect_native_build_input_paths(
            &root,
            &mut cargo_walk,
            &mut cargo_files,
            NativeInputTreeKind::CargoPackage,
            target_directory,
        )?;
    }
    for directory in known_cargo_dirs {
        if !cargo_walk.directory_entries.contains_key(&directory) {
            return Err(native_build_input_read_error(
                &directory,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native Cargo input directory generation changed during capability capture",
                ),
            ));
        }
    }
    if module_generation_entries
        .saturating_add(cargo_walk.directory_entries.len())
        .saturating_add(cargo_walk.captured_files.len())
        .saturating_add(files.len())
        > NATIVE_EXTENSION_MAX_INPUT_ENTRIES
    {
        return Err(native_module_input_entry_limit_error(Path::new(
            "native Cargo inputs",
        )));
    }

    hash_native_input_walk(&mut hasher, "cargo", &cargo_walk)?;
    drop(cargo_walk);
    drop(cargo_files);
    for path in files {
        match super::host_input::read_stable_regular_file_snapshot(
            &path,
            NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES,
        ) {
            Ok(snapshot) => {
                hasher.update_path("extra_file", &path);
                hash_host_metadata_generation(
                    &mut hasher,
                    "extra_generation",
                    &snapshot.generation,
                );
                hasher.update_bytes("extra_bytes", &snapshot.bytes);
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(native_build_input_read_error(&path, error)),
        }
    }
    Ok(hasher.finish())
}

fn hash_native_input_namespace_generations(
    hasher: &mut StableHasher,
    module_roots: &[PathBuf],
    cargo_roots: &[PathBuf],
    files: &[PathBuf],
) -> Result<(), ModuleSystemError> {
    // File and tree generations identify opened objects. Their immediate
    // parent generation additionally binds each lexical name, catching a
    // rename-away/build/rename-back ABA without making unrelated changes in
    // distant filesystem ancestors invalidate the build.
    let mut directories = BTreeSet::new();
    for root in module_roots.iter().chain(cargo_roots) {
        let absolute = std::path::absolute(root)
            .map_err(|error| native_build_input_read_error(root, error))?;
        if let Some(parent) = absolute.parent() {
            directories.insert(parent.to_path_buf());
        }
    }
    for file in files {
        let absolute = std::path::absolute(file)
            .map_err(|error| native_build_input_read_error(file, error))?;
        if let Some(parent) = absolute.parent() {
            directories.insert(parent.to_path_buf());
        }
    }
    if directories.len() > NATIVE_EXTENSION_MAX_INPUT_ENTRIES {
        return Err(native_module_input_entry_limit_error(Path::new(
            "native input namespace",
        )));
    }
    for directory in directories {
        let generation = super::host_input::validate_stable_directory_path(&directory)
            .map_err(|error| native_build_input_read_error(&directory, error))?;
        hasher.update_path("namespace_directory", &directory);
        hash_host_metadata_generation(hasher, "namespace_generation", &generation);
    }
    Ok(())
}

fn hash_native_input_walk(
    hasher: &mut StableHasher,
    tree_kind: &str,
    walk: &NativeInputWalkState,
) -> Result<(), ModuleSystemError> {
    hasher.update_str("input_tree_kind", tree_kind);
    for (path, entries) in &walk.directory_entries {
        hasher.update_path("directory", path);
        let generation = walk.directory_generations.get(path).ok_or_else(|| {
            native_build_input_read_error(
                path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native input walk did not retain the directory generation",
                ),
            )
        })?;
        hash_host_metadata_generation(hasher, "directory_generation", generation);
        for entry in entries {
            hasher.update_path("entry_name", Path::new(&entry.name));
            hasher.update_str(
                "entry_kind",
                match entry.kind {
                    HostEntryKind::RegularFile => "file",
                    HostEntryKind::Directory => "directory",
                    HostEntryKind::Symlink => "symlink",
                    HostEntryKind::Special => "special",
                },
            );
        }
    }
    for (path, bytes) in &walk.captured_files {
        hasher.update_path("captured_file", path);
        let generation = walk.file_generations.get(path).ok_or_else(|| {
            native_build_input_read_error(
                path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native input walk did not retain the file generation",
                ),
            )
        })?;
        hash_host_metadata_generation(hasher, "captured_generation", generation);
        hasher.update_bytes("captured_bytes", bytes);
    }
    Ok(())
}

fn hash_host_metadata_generation(
    hasher: &mut StableHasher,
    label: &str,
    generation: &HostMetadataGeneration,
) {
    hasher.update_str("generation_label", label);
    hasher.update_bytes(
        "generation_volume",
        &generation.identity.volume.to_le_bytes(),
    );
    hasher.update_bytes("generation_file", &generation.identity.file);
    hasher.update_str(
        "generation_kind",
        match generation.kind {
            HostEntryKind::RegularFile => "file",
            HostEntryKind::Directory => "directory",
            HostEntryKind::Symlink => "symlink",
            HostEntryKind::Special => "special",
        },
    );
    hasher.update_bytes("generation_len", &generation.len.to_le_bytes());
    hasher.update_bytes(
        "generation_modified_seconds",
        &generation.modified_seconds.to_le_bytes(),
    );
    hasher.update_bytes(
        "generation_modified_subseconds",
        &generation.modified_subseconds.to_le_bytes(),
    );
    hasher.update_bytes(
        "generation_changed_seconds",
        &generation.changed_seconds.to_le_bytes(),
    );
    hasher.update_bytes(
        "generation_changed_subseconds",
        &generation.changed_subseconds.to_le_bytes(),
    );
    hasher.update_bytes(
        "generation_created_seconds",
        &generation.created_seconds.to_le_bytes(),
    );
    hasher.update_bytes(
        "generation_created_subseconds",
        &generation.created_subseconds.to_le_bytes(),
    );
    hasher.update_bytes(
        "generation_attributes",
        &generation.attributes.to_le_bytes(),
    );
    hasher.update_bytes("generation_links", &generation.links.to_le_bytes());
    hasher.update_str(
        "generation_delete_pending",
        if generation.delete_pending {
            "true"
        } else {
            "false"
        },
    );
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
    let _mod_file = ModFile::parse(&mod_content).map_err(|error| {
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
    let context = vo_module::project::load_project_context_with_options(
        &vfs,
        &module_dir,
        &vo_module::project::ProjectContextOptions::new(workspace_discovery.clone()),
    )
    .map_err(|error| {
        let detail = format!(
            "failed to capture native extension workspace inputs for {}: {error}",
            module_dir.display(),
        );
        super::module_system_error_from_project(error).with_detail(detail)
    })?;

    let mut roots = BTreeSet::new();
    roots.insert(module_dir.clone());
    for local_dir in context.workspace_sources().values() {
        roots.insert(
            local_dir
                .canonicalize()
                .unwrap_or_else(|_| local_dir.clone()),
        );
    }
    Ok((roots, context.workspace_file().map(Path::to_path_buf)))
}

fn collect_native_module_compile_inputs_from_roots(
    module_dir: &Path,
    roots: BTreeSet<PathBuf>,
    workfile: Option<PathBuf>,
) -> Result<Vec<(PathBuf, Vec<u8>)>, ModuleSystemError> {
    if roots.len() > NATIVE_EXTENSION_MAX_INPUT_ENTRIES {
        return Err(native_module_input_entry_limit_error(module_dir));
    }
    let mut captured = BTreeMap::<PathBuf, Vec<u8>>::new();
    let mut scanned_entries = roots.len();
    let mut total_bytes = 0usize;
    for root in roots {
        let remaining_entries = NATIVE_EXTENSION_MAX_INPUT_ENTRIES.saturating_sub(scanned_entries);
        let remaining_files = NATIVE_EXTENSION_MAX_INPUT_FILES.saturating_sub(captured.len());
        let remaining_bytes = NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES.saturating_sub(total_bytes);
        let (relative_paths, root_entries) = collect_module_compile_input_files(
            &root,
            remaining_entries,
            remaining_files,
            remaining_bytes,
        )
        .map_err(|error| native_module_compile_input_error(&root, error))?;
        scanned_entries = scanned_entries
            .checked_add(root_entries)
            .ok_or_else(|| native_module_input_entry_limit_error(module_dir))?;
        if scanned_entries > NATIVE_EXTENSION_MAX_INPUT_ENTRIES {
            return Err(native_module_input_entry_limit_error(module_dir));
        }
        for (relative_path, bytes) in relative_paths {
            let path = root.join(relative_path);
            if let Some(existing) = captured.get(&path) {
                if existing != &bytes {
                    return Err(native_build_input_read_error(
                        &path,
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "native extension module input changed across overlapping capability walks",
                        ),
                    ));
                }
                continue;
            }
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
            captured.insert(path, bytes);
            if captured.len() > NATIVE_EXTENSION_MAX_INPUT_FILES {
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
        let existing = captured.get(&workfile);
        if existing.is_none() && captured.len() >= NATIVE_EXTENSION_MAX_INPUT_FILES {
            return Err(native_module_input_file_limit_error(module_dir));
        }
        let max_bytes = if existing.is_some() {
            NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES
        } else {
            NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES
                .min(NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES.saturating_sub(total_bytes))
        };
        let bytes = read_bounded_file(&workfile, max_bytes, "native extension workspace input")
            .map_err(|error| native_build_input_read_error(&workfile, error))?;
        if let Some(existing) = existing {
            if existing != &bytes {
                return Err(native_build_input_read_error(
                    &workfile,
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "native extension workspace input changed across overlapping capability walks",
                    ),
                ));
            }
        } else {
            captured.insert(workfile, bytes);
        }
        if captured.len() > NATIVE_EXTENSION_MAX_INPUT_FILES {
            return Err(native_module_input_file_limit_error(module_dir));
        }
    }

    let mut inputs = Vec::with_capacity(captured.len());
    for (path, bytes) in captured {
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

fn native_module_compile_input_error(root: &Path, error: super::CompileError) -> ModuleSystemError {
    let kind = match &error {
        super::CompileError::Io(error) => native_input_error_kind(error),
        super::CompileError::ModuleSystem(error) => error.kind,
        super::CompileError::Parse(_) => ModuleSystemErrorKind::ParseFailed,
        super::CompileError::Analysis(_) | super::CompileError::Codegen(_) => {
            ModuleSystemErrorKind::ValidationFailed
        }
    };
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        kind,
        format!(
            "failed to capture native extension module inputs at {}: {}",
            root.display(),
            error,
        ),
    )
    .with_path(root)
}

fn collect_native_build_inputs(
    manifest_dir: &Path,
    target_directory: &Path,
) -> Result<Vec<(PathBuf, Vec<u8>)>, ModuleSystemError> {
    let mut paths = Vec::new();
    let mut walk = NativeInputWalkState::default();
    collect_native_build_input_paths(
        manifest_dir,
        &mut walk,
        &mut paths,
        NativeInputTreeKind::CargoPackage,
        target_directory,
    )?;
    paths.sort();
    let mut total_bytes = 0usize;
    let mut inputs = Vec::with_capacity(paths.len());
    for path in paths {
        let relative = path
            .strip_prefix(manifest_dir)
            .unwrap_or(&path)
            .to_path_buf();
        let bytes = walk.captured_files.remove(&path).ok_or_else(|| {
            native_build_input_read_error(
                &path,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native input walk did not retain the opened file bytes",
                ),
            )
        })?;
        total_bytes = total_bytes.checked_add(bytes.len()).ok_or_else(|| {
            native_build_input_read_error(
                manifest_dir,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native extension input tree size overflow",
                ),
            )
        })?;
        if total_bytes > NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES {
            return Err(native_build_input_read_error(
                manifest_dir,
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
    directory_identities: BTreeSet<HostEntryIdentity>,
    directory_entries: BTreeMap<PathBuf, Vec<super::host_input::HostDirectoryEntry>>,
    directory_generations: BTreeMap<PathBuf, HostMetadataGeneration>,
    captured_files: BTreeMap<PathBuf, Vec<u8>>,
    file_generations: BTreeMap<PathBuf, HostMetadataGeneration>,
    entries: usize,
    entry_limit: usize,
    file_limit: usize,
    bytes: usize,
    byte_limit: usize,
}

impl NativeInputWalkState {
    fn with_entry_limit(entry_limit: usize) -> Self {
        Self::with_limits(
            entry_limit,
            NATIVE_EXTENSION_MAX_INPUT_FILES,
            NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES,
        )
    }

    fn with_limits(entry_limit: usize, file_limit: usize, byte_limit: usize) -> Self {
        Self {
            visited_dirs: BTreeSet::new(),
            directory_identities: BTreeSet::new(),
            directory_entries: BTreeMap::new(),
            directory_generations: BTreeMap::new(),
            captured_files: BTreeMap::new(),
            file_generations: BTreeMap::new(),
            entries: 0,
            entry_limit,
            file_limit,
            bytes: 0,
            byte_limit,
        }
    }
}

impl Default for NativeInputWalkState {
    fn default() -> Self {
        Self::with_entry_limit(NATIVE_EXTENSION_MAX_INPUT_ENTRIES)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeInputTreeKind {
    CargoPackage,
    ModuleTree,
}

fn collect_native_build_input_paths(
    root: &Path,
    walk: &mut NativeInputWalkState,
    out: &mut Vec<PathBuf>,
    tree_kind: NativeInputTreeKind,
    target_directory: &Path,
) -> Result<(), ModuleSystemError> {
    collect_native_build_input_paths_with_opaque_root(
        root,
        walk,
        out,
        tree_kind,
        target_directory,
        None,
    )
}

fn collect_native_build_input_paths_with_opaque_root(
    root: &Path,
    walk: &mut NativeInputWalkState,
    out: &mut Vec<PathBuf>,
    tree_kind: NativeInputTreeKind,
    target_directory: &Path,
    opaque_native_root: Option<&Path>,
) -> Result<(), ModuleSystemError> {
    let root_directory = super::host_input::read_stable_directory(
        root,
        walk.entry_limit.saturating_sub(walk.entries),
    )
    .map_err(|error| native_build_input_read_error(root, error))?;
    let mut pending = vec![(root.to_path_buf(), 0, root_directory)];
    while let Some((dir, depth, directory)) = pending.pop() {
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
        if !walk.directory_identities.insert(directory.identity.clone()) {
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
        walk.visited_dirs.insert(dir.clone());
        let directory_capability = directory.capability.clone();
        let entries = directory.entries;
        walk.directory_generations
            .insert(dir.clone(), directory.generation);
        walk.directory_entries.insert(dir.clone(), entries.clone());
        walk.entries = walk.entries.checked_add(entries.len()).ok_or_else(|| {
            native_build_input_read_error(
                root,
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
        let mut child_dirs = Vec::new();
        for entry in entries {
            let path = dir.join(&entry.name);
            match entry.kind {
                HostEntryKind::Directory => {
                    if should_skip_native_build_input_dir(&path, tree_kind, target_directory)
                        .map_err(|error| native_build_input_read_error(&path, error))?
                    {
                        continue;
                    }
                    if super::host_input::child_is_declared_cache_directory(
                        &directory_capability,
                        &entry.name,
                        &path,
                    )
                    .map_err(|error| native_build_input_read_error(&path, error))?
                    {
                        continue;
                    }
                    let child = super::host_input::read_stable_directory_child(
                        &directory_capability,
                        &entry.name,
                        &path,
                        walk.entry_limit.saturating_sub(walk.entries),
                    )
                    .map_err(|error| native_build_input_read_error(&path, error))?;
                    child_dirs.push((path, child));
                }
                HostEntryKind::RegularFile => {
                    let inside_opaque_root = opaque_native_root
                        .map(|root| super::host_input::portable_host_path_starts_with(&path, root))
                        .transpose()
                        .map_err(|error| native_build_input_read_error(&path, error))?
                        .unwrap_or(false);
                    if inside_opaque_root
                        && is_opaque_native_protocol_file(&path)
                            .map_err(|error| native_build_input_read_error(&path, error))?
                    {
                        return Err(forbidden_opaque_native_protocol_file_error(
                            opaque_native_root.expect("opaque-root membership requires a root"),
                            &path,
                        ));
                    }
                    if tree_kind == NativeInputTreeKind::ModuleTree
                        && should_skip_native_build_output_file(&path)
                            .map_err(|error| native_build_input_read_error(&path, error))?
                    {
                        continue;
                    }
                    let existing = walk.captured_files.get(&path);
                    if existing.is_none() && walk.captured_files.len() >= walk.file_limit {
                        return Err(native_build_input_read_error(
                            root,
                            std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                format!(
                                    "native extension input tree exceeds the {}-file limit",
                                    walk.file_limit,
                                ),
                            ),
                        ));
                    }
                    let max_bytes = if existing.is_some() {
                        NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES
                    } else {
                        NATIVE_EXTENSION_MAX_INPUT_FILE_BYTES
                            .min(walk.byte_limit.saturating_sub(walk.bytes))
                    };
                    let file_snapshot = super::host_input::read_stable_regular_child_snapshot(
                        &directory_capability,
                        &entry.name,
                        &path,
                        max_bytes,
                    )
                    .map_err(|error| native_build_input_read_error(&path, error))?;
                    let bytes = file_snapshot.bytes;
                    if let Some(existing) = walk.captured_files.get(&path) {
                        if existing != &bytes
                            || walk.file_generations.get(&path) != Some(&file_snapshot.generation)
                        {
                            return Err(native_build_input_read_error(
                                &path,
                                std::io::Error::new(
                                    std::io::ErrorKind::InvalidData,
                                    "native input changed across overlapping capability walks",
                                ),
                            ));
                        }
                    } else {
                        walk.bytes = walk.bytes.checked_add(bytes.len()).ok_or_else(|| {
                            native_build_input_read_error(
                                root,
                                std::io::Error::new(
                                    std::io::ErrorKind::InvalidData,
                                    "native extension input tree size overflow",
                                ),
                            )
                        })?;
                        if walk.bytes > walk.byte_limit {
                            return Err(native_build_input_read_error(
                                root,
                                std::io::Error::new(
                                    std::io::ErrorKind::InvalidData,
                                    format!(
                                        "native extension input tree exceeds the {}-byte limit",
                                        walk.byte_limit,
                                    ),
                                ),
                            ));
                        }
                        walk.captured_files.insert(path.clone(), bytes);
                        walk.file_generations
                            .insert(path.clone(), file_snapshot.generation);
                    }
                    out.push(path);
                }
                HostEntryKind::Symlink | HostEntryKind::Special => {
                    return Err(native_build_input_read_error(
                        &path,
                        unsupported_native_input_type_error(&path),
                    ));
                }
            }
        }
        for (child, directory) in child_dirs.into_iter().rev() {
            pending.push((child, depth.saturating_add(1), directory));
        }
        debug_assert!(dir == root || dir.starts_with(root));
    }
    Ok(())
}

fn is_opaque_native_protocol_file(path: &Path) -> std::io::Result<bool> {
    let Some(name) = path.file_name() else {
        return Ok(false);
    };
    let key = super::host_input::portable_host_name_key(name, path)?;
    Ok(key.ends_with(".vo") || matches!(key.as_str(), "vo.mod" | "vo.lock" | "vo.work"))
}

fn forbidden_opaque_native_protocol_file_error(
    opaque_native_root: &Path,
    path: &Path,
) -> ModuleSystemError {
    native_build_input_read_error(
        path,
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "opaque native build root {} contains forbidden Vo source or module-control file {} (`*.vo`, `vo.mod`, `vo.lock`, and `vo.work` are reserved for the language tree)",
                opaque_native_root.display(),
                path.display(),
            ),
        ),
    )
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

fn should_skip_native_build_input_dir(
    path: &Path,
    tree_kind: NativeInputTreeKind,
    target_directory: &Path,
) -> std::io::Result<bool> {
    if super::host_input::portable_host_path_eq(path, target_directory)? {
        return Ok(true);
    }
    let Some(name) = path.file_name() else {
        return Ok(false);
    };
    for excluded in [".git", ".volang", ".vo-cache", "target"] {
        if super::host_input::portable_host_name_eq(name, excluded, path)? {
            return Ok(true);
        }
    }
    // A module source tree never treats dependency-manager materialization as
    // language input. Cargo packages retain node_modules because build.rs and
    // proc macros may legally consume checked-in JavaScript tool inputs.
    Ok(tree_kind == NativeInputTreeKind::ModuleTree
        && super::host_input::portable_host_name_eq(name, "node_modules", path)?)
}

fn should_skip_native_build_output_file(path: &Path) -> std::io::Result<bool> {
    let Some(name) = path.file_name() else {
        return Ok(false);
    };
    let key = super::host_input::portable_host_name_key(name, path)?;
    Ok(key.contains(".voabi-")
        || key.ends_with(".vo-abi")
        || key.ends_with(".vo-inputs")
        || key.starts_with(".vo-build-lock-"))
}

fn native_build_input_read_error(path: &Path, error: std::io::Error) -> ModuleSystemError {
    let kind = native_input_error_kind(&error);
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        kind,
        format!(
            "failed to read native extension build input at {}: {}",
            path.display(),
            error,
        ),
    )
    .with_path(path)
}

fn native_input_error_kind(error: &std::io::Error) -> ModuleSystemErrorKind {
    match error.kind() {
        std::io::ErrorKind::NotFound => ModuleSystemErrorKind::Missing,
        std::io::ErrorKind::InvalidData
        | std::io::ErrorKind::InvalidInput
        | std::io::ErrorKind::Unsupported => ModuleSystemErrorKind::ValidationFailed,
        _ => ModuleSystemErrorKind::ReadFailed,
    }
}

fn read_bounded_file(path: &Path, max_bytes: usize, kind: &str) -> std::io::Result<Vec<u8>> {
    super::host_input::read_stable_regular_file(path, max_bytes).map_err(|error| {
        std::io::Error::new(
            error.kind(),
            format!("failed to read {kind} at {}: {error}", path.display()),
        )
    })
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
    run_command_with_bounded_output_and_spawner(
        command,
        max_bytes_per_stream,
        spawn_bounded_command_output_reader,
    )
}

type CommandOutputReader = std::thread::JoinHandle<std::io::Result<Vec<u8>>>;
type CommandOutputPipe = Box<dyn Read + Send>;

fn run_command_with_bounded_output_and_spawner<F>(
    command: &mut std::process::Command,
    max_bytes_per_stream: usize,
    mut spawn_reader: F,
) -> std::io::Result<BoundedCommandOutput>
where
    F: FnMut(CommandOutputPipe, usize, &'static str) -> std::io::Result<CommandOutputReader>,
{
    command
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    let mut child = command.spawn()?;
    let stdout = match child.stdout.take() {
        Some(stdout) => Box::new(stdout) as CommandOutputPipe,
        None => {
            let cleanup = cleanup_command_after_reader_start_failure(&mut child, Vec::new());
            return Err(command_output_reader_start_error(
                "stdout",
                std::io::Error::other("spawned command did not expose its piped stdout"),
                cleanup,
            ));
        }
    };
    let stderr = match child.stderr.take() {
        Some(stderr) => Box::new(stderr) as CommandOutputPipe,
        None => {
            drop(stdout);
            let cleanup = cleanup_command_after_reader_start_failure(&mut child, Vec::new());
            return Err(command_output_reader_start_error(
                "stderr",
                std::io::Error::other("spawned command did not expose its piped stderr"),
                cleanup,
            ));
        }
    };
    let stdout_reader = match spawn_reader(stdout, max_bytes_per_stream, "stdout") {
        Ok(reader) => reader,
        Err(error) => {
            drop(stderr);
            let cleanup = cleanup_command_after_reader_start_failure(&mut child, Vec::new());
            return Err(command_output_reader_start_error("stdout", error, cleanup));
        }
    };
    let stderr_reader = match spawn_reader(stderr, max_bytes_per_stream, "stderr") {
        Ok(reader) => reader,
        Err(error) => {
            let cleanup =
                cleanup_command_after_reader_start_failure(&mut child, vec![stdout_reader]);
            return Err(command_output_reader_start_error("stderr", error, cleanup));
        }
    };
    let status = child.wait();
    let stdout = join_command_output_reader(stdout_reader);
    let stderr = join_command_output_reader(stderr_reader);
    Ok(BoundedCommandOutput {
        status: status?,
        stdout: stdout?,
        stderr: stderr?,
    })
}

fn spawn_bounded_command_output_reader(
    reader: CommandOutputPipe,
    max_bytes: usize,
    stream: &'static str,
) -> std::io::Result<CommandOutputReader> {
    std::thread::Builder::new()
        .name(format!("vo-command-{stream}-reader"))
        .spawn(move || read_bounded_command_pipe(reader, max_bytes, stream))
}

fn cleanup_command_after_reader_start_failure(
    child: &mut std::process::Child,
    readers: Vec<CommandOutputReader>,
) -> Vec<String> {
    let mut cleanup_errors = Vec::new();
    if let Err(error) = child.kill() {
        if error.kind() != std::io::ErrorKind::InvalidInput {
            cleanup_errors.push(format!("failed to terminate command: {error}"));
        }
    }
    if let Err(error) = child.wait() {
        cleanup_errors.push(format!("failed to reap command: {error}"));
    }
    for reader in readers {
        if let Err(error) = join_command_output_reader(reader) {
            cleanup_errors.push(format!(
                "failed to finish an already-started reader: {error}"
            ));
        }
    }
    cleanup_errors
}

fn command_output_reader_start_error(
    stream: &str,
    error: std::io::Error,
    cleanup_errors: Vec<String>,
) -> std::io::Error {
    let kind = error.kind();
    let mut detail = format!("failed to start bounded command {stream} reader: {error}");
    if !cleanup_errors.is_empty() {
        detail.push_str("; cleanup also reported: ");
        detail.push_str(&cleanup_errors.join("; "));
    }
    std::io::Error::new(kind, detail)
}

fn native_cargo_command_start_error(
    action: &str,
    path: &Path,
    kind: ModuleSystemErrorKind,
    error: std::io::Error,
) -> ModuleSystemError {
    let path_is_configured = std::env::var_os("PATH").is_some_and(|value| !value.is_empty());
    let detail = native_cargo_command_start_detail(action, &error, path_is_configured);
    ModuleSystemError::new(ModuleSystemStage::NativeExtension, kind, detail).with_path(path)
}

fn native_cargo_command_start_detail(
    action: &str,
    error: &std::io::Error,
    path_is_configured: bool,
) -> String {
    if error.kind() != std::io::ErrorKind::NotFound {
        return format!("failed to start `cargo {action}`: {error}");
    }
    if path_is_configured {
        format!(
            "failed to start `cargo {action}`: Cargo was not found through PATH, or its working directory became unavailable: {error}"
        )
    } else {
        format!(
            "failed to start `cargo {action}`: PATH is unset or empty, so Cargo cannot be discovered for a native extension build: {error}"
        )
    }
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

pub(super) fn native_build_context_fingerprint_for_manifest(
    module_dir: &Path,
    cargo_manifest: &Path,
) -> Result<String, ModuleSystemError> {
    let (manifest_path, manifest_dir) =
        canonical_native_cargo_manifest(module_dir, cargo_manifest)?;
    let mut hasher = StableHasher::new("vo-native-extension-build-context-v2");
    hasher.update_path("cargo_manifest", &manifest_path);
    hasher.update_path("manifest_dir", &manifest_dir);
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
    hasher.update_str("tool_identity", &native_tool_identity(&manifest_dir)?);

    for (path, bytes) in
        read_native_build_context_inputs(native_build_context_files(&manifest_dir)?, &manifest_dir)?
    {
        hasher.update_path("context_path", &path);
        hasher.update_bytes("context_bytes", &bytes);
    }

    let mut environment = std::env::vars_os()
        .filter(|(key, _)| {
            let key = key.as_os_str();
            key != std::ffi::OsStr::new(NATIVE_EXTENSION_FFI_SOURCE_FINGERPRINT_ENV)
                && key != std::ffi::OsStr::new("VOWORK")
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

fn native_build_context_files(manifest_dir: &Path) -> Result<Vec<PathBuf>, ModuleSystemError> {
    let mut files = BTreeSet::new();
    for ancestor in manifest_dir.ancestors() {
        for relative in [
            "Cargo.toml",
            "Cargo.lock",
            "rust-toolchain.toml",
            "rust-toolchain",
            ".cargo/config.toml",
            ".cargo/config",
        ] {
            insert_existing_context_file(&mut files, ancestor.join(relative))?;
        }
    }

    if let Some(cargo_home) = cargo_home_dir() {
        insert_existing_context_file(&mut files, cargo_home.join("config.toml"))?;
        insert_existing_context_file(&mut files, cargo_home.join("config"))?;
    }

    Ok(files.into_iter().collect())
}

fn native_cargo_context_files(
    cargo: &NativeCargoContext,
) -> Result<Vec<PathBuf>, ModuleSystemError> {
    let mut files = native_build_context_files(&cargo.manifest_dir)?
        .into_iter()
        .collect::<BTreeSet<_>>();
    files.extend(native_build_context_files(&cargo.workspace_root)?);
    files.insert(cargo.manifest_path.clone());
    files.insert(cargo.lock_path.clone());
    Ok(files.into_iter().collect())
}

fn read_native_build_context_inputs(
    files: Vec<PathBuf>,
    context_root: &Path,
) -> Result<Vec<(PathBuf, Vec<u8>)>, ModuleSystemError> {
    if files.len() > NATIVE_EXTENSION_MAX_INPUT_FILES {
        return Err(native_module_input_file_limit_error(context_root));
    }
    let mut context_bytes = 0usize;
    let mut inputs = Vec::with_capacity(files.len());
    for path in files {
        let remaining_bytes = NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES.saturating_sub(context_bytes);
        let bytes = read_bounded_file(
            &path,
            MAX_TEXT_FILE_BYTES.min(remaining_bytes),
            "native build context",
        )
        .map_err(|error| native_build_input_read_error(&path, error))?;
        context_bytes = context_bytes.checked_add(bytes.len()).ok_or_else(|| {
            native_build_input_read_error(
                context_root,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "native build context size overflow",
                ),
            )
        })?;
        if context_bytes > NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES {
            return Err(native_build_input_read_error(
                context_root,
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!(
                        "native build context exceeds the {}-byte limit",
                        NATIVE_EXTENSION_MAX_INPUT_TREE_BYTES,
                    ),
                ),
            ));
        }
        inputs.push((path, bytes));
    }
    Ok(inputs)
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
                let name = path.file_name().ok_or_else(|| {
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!(
                            "local Volang patch input has no file name: {}",
                            path.display()
                        ),
                    )
                })?;
                let mut skipped = false;
                for excluded in ["target", ".git", "pkg", "pkg-island", "node_modules"] {
                    if super::host_input::portable_host_name_eq(name, excluded, &path)? {
                        skipped = true;
                        break;
                    }
                }
                if !skipped {
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

fn insert_existing_context_file(
    files: &mut BTreeSet<PathBuf>,
    path: PathBuf,
) -> Result<(), ModuleSystemError> {
    match fs::symlink_metadata(&path) {
        Ok(metadata) if metadata.file_type().is_file() => {
            // Keep the lexical path Cargo will read. The bounded context read
            // opens every component without following links, so retaining a
            // canonicalized target here would lose the provenance of a
            // replaceable config symlink.
            files.insert(path);
            Ok(())
        }
        Ok(_) => Err(native_build_input_read_error(
            &path,
            unsupported_native_input_type_error(&path),
        )),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(native_build_input_read_error(&path, error)),
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

fn native_tool_identity(manifest_dir: &Path) -> Result<String, ModuleSystemError> {
    let mut hasher = StableHasher::new("vo-native-extension-tools-v1");
    for tool in ["cargo", "rustc"] {
        let mut command = std::process::Command::new(tool);
        command
            .args(["--version", "--verbose"])
            .current_dir(manifest_dir);
        let output = run_command_with_bounded_output(&mut command, MAX_TEXT_FILE_BYTES).map_err(
            |error| {
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::ReadFailed,
                    format!("failed to inspect {tool}: {error}"),
                )
                .with_path(manifest_dir)
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
            .with_path(manifest_dir));
        }
        hasher.update_str("tool", tool);
        hasher.update_bytes("stdout", &output.stdout);
        hasher.update_bytes("stderr", &output.stderr);
    }
    Ok(hasher.finish())
}

fn materialize_native_extension_load_copy(
    native_path: &Path,
) -> Result<NativeLoadCopy, ModuleSystemError> {
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
    materialize_native_extension_load_copy_from_bytes(native_path, &bytes)
}

fn materialize_native_extension_load_copy_from_bytes(
    native_path: &Path,
    bytes: &[u8],
) -> Result<NativeLoadCopy, ModuleSystemError> {
    if bytes.len() > NATIVE_EXTENSION_MAX_ARTIFACT_BYTES {
        return Err(native_build_input_read_error(
            native_path,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "native extension artifact exceeds the {}-byte limit",
                    NATIVE_EXTENSION_MAX_ARTIFACT_BYTES,
                ),
            ),
        ));
    }
    let store_lock_path = native_extension_load_store_lock_path(native_path);
    let store_lock = native_extension_load_store_lock(&store_lock_path);
    let _store_guard = store_lock
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let _cross_process_store_guard =
        acquire_native_extension_load_store_file_lock(&store_lock_path)?;

    let load_path = native_extension_load_copy_path_for_bytes(native_path, bytes);
    let expected_load_marker = native_extension_abi_marker_for_bytes(bytes);
    match fs::symlink_metadata(&load_path) {
        Ok(_) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            publish_native_file_if_absent(&load_path, bytes).map_err(|error| {
                native_load_copy_io_error(&load_path, "publish load copy", error)
            })?;
        }
        Err(error) => {
            return Err(native_load_copy_io_error(
                &load_path,
                "inspect load copy",
                error,
            ));
        }
    }
    validate_content_addressed_native_load_copy_artifact(&load_path, bytes)?;
    validate_native_lock_path_identity(&_cross_process_store_guard, &store_lock_path).map_err(
        |error| {
            native_load_copy_io_error(
                &store_lock_path,
                "revalidate load store before marker publication",
                error,
            )
        },
    )?;
    let marker_path = native_extension_abi_marker_path(&load_path);
    match fs::symlink_metadata(&marker_path) {
        Ok(_) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            // Publication is deliberately recoverable across a crash between
            // the artifact and marker creates. Exact artifact bytes have
            // already been authenticated under the store lock.
            publish_native_file_if_absent(&marker_path, expected_load_marker.as_bytes()).map_err(
                |error| native_load_copy_io_error(&marker_path, "publish ABI marker", error),
            )?;
        }
        Err(error) => {
            return Err(native_load_copy_io_error(
                &marker_path,
                "inspect ABI marker",
                error,
            ));
        }
    }
    validate_content_addressed_native_load_copy(&load_path, bytes, &expected_load_marker)?;

    let lease = acquire_native_extension_load_copy_lease(&load_path)?;
    validate_native_lock_path_identity(&_cross_process_store_guard, &store_lock_path).map_err(
        |error| {
            native_load_copy_io_error(
                &store_lock_path,
                "revalidate load store before collection",
                error,
            )
        },
    )?;
    prune_inactive_native_extension_load_copies(
        native_path,
        &load_path,
        &_cross_process_store_guard,
        &store_lock_path,
    )?;
    Ok(NativeLoadCopy {
        path: load_path,
        lease,
    })
}

fn validate_content_addressed_native_load_copy(
    load_path: &Path,
    expected_bytes: &[u8],
    expected_marker: &str,
) -> Result<(), ModuleSystemError> {
    validate_content_addressed_native_load_copy_artifact(load_path, expected_bytes)?;
    let marker_path = native_extension_abi_marker_path(load_path);
    let actual_marker = read_bounded_text_file(
        &marker_path,
        NATIVE_EXTENSION_MAX_MARKER_BYTES,
        "content-addressed native extension ABI marker",
    )
    .map_err(|error| content_addressed_native_load_copy_error(load_path, error.to_string()))?;
    if actual_marker != expected_marker {
        return Err(content_addressed_native_load_copy_error(
            load_path,
            "ABI marker does not match the content-addressed artifact",
        ));
    }
    Ok(())
}

fn validate_content_addressed_native_load_copy_artifact(
    load_path: &Path,
    expected_bytes: &[u8],
) -> Result<(), ModuleSystemError> {
    let actual_bytes = read_bounded_file(
        load_path,
        NATIVE_EXTENSION_MAX_ARTIFACT_BYTES,
        "content-addressed native extension load copy",
    )
    .map_err(|error| content_addressed_native_load_copy_error(load_path, error.to_string()))?;
    if actual_bytes != expected_bytes {
        return Err(content_addressed_native_load_copy_error(
            load_path,
            "artifact bytes do not match the digest in the file name",
        ));
    }
    Ok(())
}

fn content_addressed_native_load_copy_error(
    load_path: &Path,
    detail: impl std::fmt::Display,
) -> ModuleSystemError {
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::BuildFailed,
        format!(
            "content-addressed native extension load copy at {} is inconsistent ({detail}); refusing to replace an existing generation",
            load_path.display(),
        ),
    )
    .with_path(load_path)
}

fn native_load_copy_io_error(
    path: &Path,
    action: &str,
    error: std::io::Error,
) -> ModuleSystemError {
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::BuildFailed,
        format!(
            "failed to {action} for a native extension at {}: {error}",
            path.display(),
        ),
    )
    .with_path(path)
}

fn native_extension_load_store_lock_path(native_path: &Path) -> PathBuf {
    let parent = native_path.parent().unwrap_or_else(|| Path::new("."));
    let (prefix, suffix) = native_extension_load_copy_family(native_path);
    let mut hasher = StableHasher::new("vo-native-extension-load-store-lock-v1");
    hasher.update_str("load_prefix", &prefix);
    hasher.update_str("load_suffix", &suffix);
    parent.join(format!(
        ".vo-native-load-store-{}.lock",
        hasher.finish_suffix(),
    ))
}

fn native_extension_load_store_lock(lock_path: &Path) -> Arc<Mutex<()>> {
    let key = lock_path
        .parent()
        .and_then(|parent| parent.canonicalize().ok())
        .map(|parent| {
            lock_path
                .file_name()
                .map_or(parent.clone(), |name| parent.join(name))
        })
        .unwrap_or_else(|| lock_path.to_path_buf());
    let locks = NATIVE_LOAD_STORE_LOCKS.get_or_init(|| Mutex::new(HashMap::new()));
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

fn acquire_native_extension_load_store_file_lock(
    lock_path: &Path,
) -> Result<fs::File, ModuleSystemError> {
    let mut options = OpenOptions::new();
    options.create(true).truncate(false).read(true).write(true);
    configure_native_lock_open_options(&mut options);
    let file = options
        .open(lock_path)
        .map_err(|error| native_load_copy_io_error(lock_path, "open load-store lock", error))?;
    validate_open_native_lock_file(&file, lock_path)
        .map_err(|error| native_load_copy_io_error(lock_path, "validate load-store lock", error))?;
    file.lock()
        .map_err(|error| native_load_copy_io_error(lock_path, "lock load store", error))?;
    validate_native_lock_path_identity(&file, lock_path).map_err(|error| {
        native_load_copy_io_error(lock_path, "revalidate locked load store", error)
    })?;
    Ok(file)
}

fn configure_native_lock_open_options(options: &mut OpenOptions) {
    #[cfg(all(unix, not(target_arch = "wasm32")))]
    {
        use std::os::unix::fs::OpenOptionsExt as _;
        options.custom_flags(libc::O_NOFOLLOW);
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::OpenOptionsExt as _;
        use windows_sys::Win32::Storage::FileSystem::{
            FILE_FLAG_OPEN_REPARSE_POINT, FILE_SHARE_READ, FILE_SHARE_WRITE,
        };
        options
            .custom_flags(FILE_FLAG_OPEN_REPARSE_POINT)
            .share_mode(FILE_SHARE_READ | FILE_SHARE_WRITE);
    }
}

fn validate_open_native_lock_file(file: &fs::File, path: &Path) -> std::io::Result<()> {
    let metadata = file.metadata()?;
    if !metadata.file_type().is_file() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "native extension lock is not a regular file: {}",
                path.display()
            ),
        ));
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::MetadataExt as _;
        use windows_sys::Win32::Storage::FileSystem::FILE_ATTRIBUTE_REPARSE_POINT;
        if metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "native extension lock cannot be a reparse point: {}",
                    path.display(),
                ),
            ));
        }
    }
    Ok(())
}

fn validate_native_lock_path_identity(file: &fs::File, path: &Path) -> std::io::Result<()> {
    super::host_input::validate_open_regular_file_path_identity(file, path).map(|_| ())
}

fn native_extension_load_copy_lease_path(load_path: &Path) -> PathBuf {
    let name = load_path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("native-extension");
    load_path.with_file_name(format!("{name}.vo-lease"))
}

fn open_native_extension_load_copy_lease_file(load_path: &Path) -> std::io::Result<fs::File> {
    let lease_path = native_extension_load_copy_lease_path(load_path);
    match fs::symlink_metadata(&lease_path) {
        Ok(metadata) if !metadata.file_type().is_file() => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "native extension load-copy lease is not a regular file: {}",
                    lease_path.display(),
                ),
            ));
        }
        Ok(_) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(error),
    }
    let mut options = OpenOptions::new();
    options.create(true).truncate(false).read(true).write(true);
    configure_native_lock_open_options(&mut options);
    let file = options.open(&lease_path)?;
    validate_open_native_lock_file(&file, &lease_path)?;
    Ok(file)
}

fn acquire_native_extension_load_copy_lease(
    load_path: &Path,
) -> Result<Arc<NativeLoadCopyLease>, ModuleSystemError> {
    let lease_path = native_extension_load_copy_lease_path(load_path);
    let file = open_native_extension_load_copy_lease_file(load_path)
        .map_err(|error| native_load_copy_io_error(&lease_path, "open load-copy lease", error))?;
    file.lock_shared()
        .map_err(|error| native_load_copy_io_error(&lease_path, "lock load-copy lease", error))?;
    validate_native_lock_path_identity(&file, &lease_path).map_err(|error| {
        native_load_copy_io_error(&lease_path, "revalidate locked load-copy lease", error)
    })?;
    Ok(Arc::new(NativeLoadCopyLease { _file: file }))
}

fn prune_inactive_native_extension_load_copies(
    native_path: &Path,
    current_load_path: &Path,
    store_lock: &fs::File,
    store_lock_path: &Path,
) -> Result<(), ModuleSystemError> {
    let parent = native_path.parent().unwrap_or_else(|| Path::new("."));
    let (prefix, suffix) = native_extension_load_copy_family(native_path);
    let mut entries = fs::read_dir(parent)
        .map_err(|error| native_load_copy_io_error(parent, "scan load-copy store", error))?
        .take(NATIVE_EXTENSION_MAX_INPUT_ENTRIES.saturating_add(1))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| native_load_copy_io_error(parent, "scan load-copy store", error))?;
    if entries.len() > NATIVE_EXTENSION_MAX_INPUT_ENTRIES {
        return Err(native_load_copy_io_error(
            parent,
            "scan load-copy store",
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "native extension load-copy store exceeds the entry limit",
            ),
        ));
    }
    entries.sort_by_key(|entry| entry.file_name());
    for entry in &entries {
        let Some(name) = entry.file_name().to_str().map(str::to_owned) else {
            continue;
        };
        if !native_extension_load_copy_name_matches_family(&name, &prefix, &suffix) {
            continue;
        }
        let candidate = entry.path();
        if candidate == current_load_path {
            continue;
        }
        if !entry
            .file_type()
            .map_err(|error| native_load_copy_io_error(&candidate, "inspect old load copy", error))?
            .is_file()
        {
            return Err(content_addressed_native_load_copy_error(
                &candidate,
                "generation path is not a regular file",
            ));
        }
        let lease_path = native_extension_load_copy_lease_path(&candidate);
        let lease_file =
            open_native_extension_load_copy_lease_file(&candidate).map_err(|error| {
                native_load_copy_io_error(&lease_path, "open old load-copy lease", error)
            })?;
        match lease_file.try_lock() {
            Ok(()) => {}
            Err(std::fs::TryLockError::WouldBlock) => continue,
            Err(std::fs::TryLockError::Error(error)) => {
                return Err(native_load_copy_io_error(
                    &lease_path,
                    "lock old load-copy lease",
                    error,
                ));
            }
        }
        validate_native_lock_path_identity(&lease_file, &lease_path).map_err(|error| {
            native_load_copy_io_error(&lease_path, "revalidate old load-copy lease", error)
        })?;
        validate_native_lock_path_identity(store_lock, store_lock_path).map_err(|error| {
            native_load_copy_io_error(
                store_lock_path,
                "revalidate load store before deleting an old generation",
                error,
            )
        })?;
        match fs::remove_file(&candidate) {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error)
                if matches!(
                    error.kind(),
                    std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::WouldBlock
                ) =>
            {
                continue;
            }
            Err(error) => {
                return Err(native_load_copy_io_error(
                    &candidate,
                    "remove inactive load copy",
                    error,
                ));
            }
        }
        let marker_path = native_extension_abi_marker_path(&candidate);
        if let Err(error) = fs::remove_file(&marker_path) {
            if error.kind() != std::io::ErrorKind::NotFound {
                return Err(native_load_copy_io_error(
                    &marker_path,
                    "remove inactive load-copy marker",
                    error,
                ));
            }
        }
        drop(lease_file);
        if let Err(error) = fs::remove_file(&lease_path) {
            if error.kind() != std::io::ErrorKind::NotFound {
                return Err(native_load_copy_io_error(
                    &lease_path,
                    "remove inactive load-copy lease",
                    error,
                ));
            }
        }
    }
    prune_orphan_native_extension_load_metadata(
        parent,
        &prefix,
        &suffix,
        entries,
        store_lock,
        store_lock_path,
    )?;
    Ok(())
}

fn prune_orphan_native_extension_load_metadata(
    parent: &Path,
    prefix: &str,
    suffix: &str,
    entries: Vec<fs::DirEntry>,
    store_lock: &fs::File,
    store_lock_path: &Path,
) -> Result<(), ModuleSystemError> {
    'orphan: for entry in entries {
        let entry_path = entry.path();
        match fs::symlink_metadata(&entry_path) {
            Ok(_) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => {
                return Err(native_load_copy_io_error(
                    &entry_path,
                    "inspect orphan load-copy metadata",
                    error,
                ));
            }
        }
        let Some(name) = entry.file_name().to_str().map(str::to_owned) else {
            continue;
        };
        let Some(base_name) = name
            .strip_suffix(".vo-abi")
            .or_else(|| name.strip_suffix(".vo-lease"))
        else {
            continue;
        };
        if !native_extension_load_copy_name_matches_family(base_name, prefix, suffix) {
            continue;
        }
        let base_path = parent.join(base_name);
        match fs::symlink_metadata(&base_path) {
            Ok(_) => continue,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => {
                return Err(native_load_copy_io_error(
                    &base_path,
                    "inspect orphaned load-copy artifact",
                    error,
                ));
            }
        }
        let lease_path = native_extension_load_copy_lease_path(&base_path);
        let lease_file =
            open_native_extension_load_copy_lease_file(&base_path).map_err(|error| {
                native_load_copy_io_error(&lease_path, "open orphaned load-copy lease", error)
            })?;
        match lease_file.try_lock() {
            Ok(()) => {}
            Err(std::fs::TryLockError::WouldBlock) => continue,
            Err(std::fs::TryLockError::Error(error)) => {
                return Err(native_load_copy_io_error(
                    &lease_path,
                    "lock orphaned load-copy lease",
                    error,
                ));
            }
        }
        validate_native_lock_path_identity(&lease_file, &lease_path).map_err(|error| {
            native_load_copy_io_error(&lease_path, "revalidate orphaned load-copy lease", error)
        })?;
        validate_native_lock_path_identity(store_lock, store_lock_path).map_err(|error| {
            native_load_copy_io_error(
                store_lock_path,
                "revalidate load store before deleting orphaned metadata",
                error,
            )
        })?;
        let marker_path = native_extension_abi_marker_path(&base_path);
        if let Err(error) = fs::remove_file(&marker_path) {
            if matches!(
                error.kind(),
                std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::WouldBlock
            ) {
                continue 'orphan;
            }
            if error.kind() != std::io::ErrorKind::NotFound {
                return Err(native_load_copy_io_error(
                    &marker_path,
                    "remove orphaned load-copy marker",
                    error,
                ));
            }
        }
        drop(lease_file);
        if let Err(error) = fs::remove_file(&lease_path) {
            if matches!(
                error.kind(),
                std::io::ErrorKind::PermissionDenied | std::io::ErrorKind::WouldBlock
            ) {
                continue 'orphan;
            }
            if error.kind() != std::io::ErrorKind::NotFound {
                return Err(native_load_copy_io_error(
                    &lease_path,
                    "remove orphaned load-copy lease",
                    error,
                ));
            }
        }
    }
    Ok(())
}

fn native_extension_load_copy_path_for_bytes(native_path: &Path, bytes: &[u8]) -> PathBuf {
    let mut hasher = StableHasher::new("vo-native-extension-load-copy-v1");
    hasher.update_str("abi", &current_native_extension_abi_identity());
    hasher.update_bytes("dylib", bytes);
    let digest = hasher.finish_suffix();
    native_extension_load_copy_path(native_path, &digest)
}

fn native_extension_load_copy_path(native_path: &Path, copy_id: &str) -> PathBuf {
    let (prefix, suffix) = native_extension_load_copy_family(native_path);
    native_path.with_file_name(format!("{prefix}{copy_id}{suffix}"))
}

fn native_extension_load_copy_family(native_path: &Path) -> (String, String) {
    let stem = native_path
        .file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("native-extension");
    let abi = format!("{}-{ABI_FINGERPRINT:016x}", ABI_VERSION);
    let prefix = format!("{stem}.voabi-{abi}-");
    let suffix = native_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| format!(".{ext}"))
        .unwrap_or_default();
    (prefix, suffix)
}

fn native_extension_load_copy_name_matches_family(name: &str, prefix: &str, suffix: &str) -> bool {
    let Some(copy_id) = name
        .strip_prefix(prefix)
        .and_then(|name| name.strip_suffix(suffix))
    else {
        return false;
    };
    copy_id.len() == 64
        && copy_id
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
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

pub(super) fn cached_native_extension_specs_match_frozen_inputs(
    specs: &mut [NativeExtensionSpec],
    frozen_input_fs: &super::snapshot::ResolverFs,
    ready_modules: &[ReadyModule],
    mod_root: &Path,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> bool {
    let mod_root = normalize_fs_path(mod_root);
    specs.iter_mut().all(|spec| {
        let Ok(content) = frozen_input_fs.read_file(&spec.manifest_path) else {
            return false;
        };
        let Ok(manifest) = parse_ext_manifest_content(&content, &spec.manifest_path) else {
            return false;
        };
        if validate_native_extension_manifest(&manifest).is_err()
            || manifest
                .declared_native_target(current_target_triple())
                .is_none()
        {
            return false;
        }
        let Ok(module_owner) = local_extension_module_owner(&manifest) else {
            return false;
        };
        if manifest.name != spec.name || module_owner != spec.module_owner {
            return false;
        }
        let Some(module_dir) = spec.manifest_path.parent().map(normalize_fs_path) else {
            return false;
        };
        let expected = if module_dir.starts_with(&mod_root) {
            let Ok(ready) =
                ready_module_for_cached_extension(&module_dir, &mod_root, ready_modules)
            else {
                return false;
            };
            prepare_cached_extension_spec(&manifest, ready, &module_dir)
        } else {
            prepare_extension_spec(
                &manifest,
                ready_modules,
                &mod_root,
                workspace_discovery,
                Some(frozen_input_fs),
            )
        };
        let Ok(expected) = expected else {
            return false;
        };
        let matches = expected.name == spec.name
            && expected.module_owner == spec.module_owner
            && normalize_fs_path(&expected.manifest_path) == normalize_fs_path(&spec.manifest_path)
            && normalize_fs_path(&expected.native_path) == normalize_fs_path(&spec.native_path);
        if matches {
            // Persistent compile-cache records intentionally omit lifetime
            // resources. Replace a matching record with the freshly prepared
            // specification so local content-addressed copies regain leases.
            *spec = expected;
        }
        matches
    })
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
    module_owner: &str,
) -> Result<(), ModuleSystemError> {
    if let Some(detail) = native_extension_host_abi_error(native_path, module_owner) {
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum NativeFilePublishOutcome {
    Created,
    AlreadyExists,
}

fn publish_native_file_if_absent(
    path: &Path,
    bytes: &[u8],
) -> Result<NativeFilePublishOutcome, std::io::Error> {
    const TEMP_ATTEMPTS: usize = 16;
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let mut path_hasher = StableHasher::new("vo-native-extension-publish-temp-v1");
    path_hasher.update_path("destination", path);
    let path_id = path_hasher.finish_suffix();
    let mut last_collision = None;
    for _ in 0..TEMP_ATTEMPTS {
        let nonce = NATIVE_MARKER_TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let temp_path = parent.join(format!(
            ".vo-native-publish-{path_id}-{}-{nonce}.tmp",
            std::process::id(),
        ));
        let mut file = match OpenOptions::new()
            .create_new(true)
            .write(true)
            .open(&temp_path)
        {
            Ok(file) => file,
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
                last_collision = Some(error);
                continue;
            }
            Err(error) => return Err(error),
        };
        let result = (|| {
            file.write_all(bytes)?;
            file.sync_all()?;
            drop(file);
            let outcome = persist_native_file_if_absent(&temp_path, path)?;
            #[cfg(unix)]
            fs::File::open(parent)?.sync_all()?;
            Ok(outcome)
        })();
        let _ = fs::remove_file(&temp_path);
        return result;
    }
    Err(last_collision.unwrap_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::AlreadyExists,
            "could not allocate a native extension publication temporary file",
        )
    }))
}

#[cfg(not(windows))]
fn persist_native_file_if_absent(
    temp_path: &Path,
    path: &Path,
) -> std::io::Result<NativeFilePublishOutcome> {
    match fs::hard_link(temp_path, path) {
        Ok(()) => Ok(NativeFilePublishOutcome::Created),
        Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
            Ok(NativeFilePublishOutcome::AlreadyExists)
        }
        Err(error) => Err(error),
    }
}

#[cfg(windows)]
fn persist_native_file_if_absent(
    temp_path: &Path,
    path: &Path,
) -> std::io::Result<NativeFilePublishOutcome> {
    match move_native_file_windows(temp_path, path, false) {
        Ok(()) => Ok(NativeFilePublishOutcome::Created),
        Err(error) => match fs::symlink_metadata(path) {
            Ok(_) => Ok(NativeFilePublishOutcome::AlreadyExists),
            Err(inspect_error) if inspect_error.kind() == std::io::ErrorKind::NotFound => {
                Err(error)
            }
            Err(inspect_error) => Err(inspect_error),
        },
    }
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
    move_native_file_windows(temp_path, path, true)
}

#[cfg(windows)]
fn move_native_file_windows(from: &Path, to: &Path, replace_existing: bool) -> std::io::Result<()> {
    use std::iter::once;
    use std::os::windows::ffi::OsStrExt as _;
    use windows_sys::Win32::Storage::FileSystem::{
        MoveFileExW, MOVEFILE_REPLACE_EXISTING, MOVEFILE_WRITE_THROUGH,
    };

    let from = from
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let to = to
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let flags = MOVEFILE_WRITE_THROUGH
        | if replace_existing {
            MOVEFILE_REPLACE_EXISTING
        } else {
            0
        };
    let result = unsafe { MoveFileExW(from.as_ptr(), to.as_ptr(), flags) };
    if result == 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
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
        .arg("--target")
        .arg(current_target_triple())
        .arg("--message-format=json-render-diagnostics");
    for config in &cargo.patch_configs {
        command.arg("--config").arg(config);
    }
    command.arg("--locked");
    if !cfg!(debug_assertions) {
        command.arg("--release");
    }
    command
        .current_dir(&cargo.manifest_dir)
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
    input_state: &NativeExtensionInputState,
    workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
) -> Result<BuiltCargoArtifact, ModuleSystemError> {
    let manifest_dir = &input_state.cargo.manifest_dir;
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "native_extension_build_start")
            .path(manifest_dir.display().to_string()),
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
            return Err(native_cargo_command_start_error(
                "build",
                manifest_dir,
                ModuleSystemErrorKind::BuildFailed,
                e,
            ));
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
                manifest_dir.display(),
                NATIVE_EXTENSION_MAX_CARGO_BUILD_OUTPUT_BYTES,
            ),
        )
        .with_path(manifest_dir));
    }
    if !output.status.success() {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "cargo build failed for {}: {}",
                manifest_dir.display(),
                String::from_utf8_lossy(&output.stderr)
            ),
        )
        .with_path(manifest_dir));
    }
    let artifact = parse_built_cargo_artifact(&output.stdout, &input_state.cargo)?;
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "native_extension_build_done")
            .path(manifest_dir.display().to_string()),
    );
    Ok(artifact)
}

fn parse_built_cargo_artifact(
    stdout: &[u8],
    cargo: &NativeCargoContext,
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
    let artifact_path = canonical_regular_native_input(
        &artifact_path,
        NATIVE_EXTENSION_MAX_ARTIFACT_BYTES,
        "Cargo native extension artifact",
    )?;
    let target_directory = normalize_native_output_path(&cargo.target_directory)?;
    if !artifact_path.starts_with(&target_directory) {
        return Err(ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::BuildFailed,
            format!(
                "Cargo produced native extension {} outside its target directory {}",
                artifact_path.display(),
                target_directory.display(),
            ),
        )
        .with_path(&artifact_path));
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

fn local_volang_cargo_patch_configs(
    lock_bytes: &[u8],
    lock_path: &Path,
) -> Result<Vec<String>, ModuleSystemError> {
    let Some((_crates_root, packages)) = local_volang_packages() else {
        return Ok(Vec::new());
    };

    let sources = [
        "https://github.com/vo-lang/volang",
        "https://github.com/vo-lang/volang.git",
    ];
    cargo_patch_configs_for_locked_packages(&sources, &packages, lock_bytes, lock_path)
}

fn cargo_patch_configs_for_locked_packages(
    sources: &[&str],
    packages: &[(&str, PathBuf)],
    lock_bytes: &[u8],
    lock_path: &Path,
) -> Result<Vec<String>, ModuleSystemError> {
    let locked_packages = cargo_lock_patch_package_names(lock_bytes, lock_path)?;
    let mut configs = Vec::new();
    for source in sources {
        for (package, path) in packages {
            if !locked_packages.contains(*package) {
                continue;
            }
            configs.push(format!(
                "patch.{}.{}.path={}",
                toml_quote(source),
                toml_quote(package),
                toml_quote(&path.to_string_lossy()),
            ));
        }
    }
    Ok(configs)
}

fn cargo_lock_patch_package_names(
    lock_bytes: &[u8],
    lock_path: &Path,
) -> Result<BTreeSet<String>, ModuleSystemError> {
    let content = std::str::from_utf8(lock_bytes).map_err(|error| {
        invalid_cargo_lock_for_native_patch_selection(lock_path, error.to_string())
    })?;
    let mut packages = BTreeSet::new();
    let mut selected_table = None::<&str>;
    let mut current_package_name = None::<String>;
    for (line_index, raw_line) in content.lines().enumerate() {
        let line = raw_line.trim();
        if line.starts_with('[') {
            if current_package_name.is_none() {
                if let Some(table) = selected_table {
                    return Err(invalid_cargo_lock_for_native_patch_selection(
                        lock_path,
                        format!("{table} before line {} has no package name", line_index + 1,),
                    ));
                }
            }
            selected_table = match line {
                "[[package]]" => Some("[[package]]"),
                "[[patch.unused]]" => Some("[[patch.unused]]"),
                _ => None,
            };
            current_package_name = None;
            continue;
        }
        if selected_table.is_none() || !line.starts_with("name") {
            continue;
        }
        let Some(encoded_name) = line.strip_prefix("name = ") else {
            return Err(invalid_cargo_lock_for_native_patch_selection(
                lock_path,
                format!("invalid package name at line {}", line_index + 1),
            ));
        };
        if current_package_name.is_some() {
            return Err(invalid_cargo_lock_for_native_patch_selection(
                lock_path,
                format!("duplicate package name at line {}", line_index + 1),
            ));
        }
        let package_name = serde_json::from_str::<String>(encoded_name).map_err(|error| {
            invalid_cargo_lock_for_native_patch_selection(
                lock_path,
                format!("invalid package name at line {}: {error}", line_index + 1),
            )
        })?;
        if package_name.is_empty()
            || !package_name
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'))
        {
            return Err(invalid_cargo_lock_for_native_patch_selection(
                lock_path,
                format!(
                    "unsupported package name {package_name:?} at line {}",
                    line_index + 1,
                ),
            ));
        }
        packages.insert(package_name.clone());
        current_package_name = Some(package_name);
    }
    if current_package_name.is_none() {
        if let Some(table) = selected_table {
            return Err(invalid_cargo_lock_for_native_patch_selection(
                lock_path,
                format!("final {table} has no package name"),
            ));
        }
    }
    Ok(packages)
}

fn invalid_cargo_lock_for_native_patch_selection(
    lock_path: &Path,
    detail: impl std::fmt::Display,
) -> ModuleSystemError {
    ModuleSystemError::new(
        ModuleSystemStage::NativeExtension,
        ModuleSystemErrorKind::ValidationFailed,
        format!(
            "failed to select local Volang Cargo patches from {}: {}",
            lock_path.display(),
            detail,
        ),
    )
    .with_path(lock_path)
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
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering as AtomicOrdering};
    use std::sync::Barrier;
    use std::time::SystemTime;
    use vo_module::ext_manifest::{
        ExtensionBuildManifest, ExtensionManifest, NativeBuildManifest, NativeExtensionManifest,
    };

    fn temp_dir(prefix: &str) -> PathBuf {
        std::env::temp_dir()
            .canonicalize()
            .unwrap_or_else(|_| std::env::temp_dir())
            .join(format!(
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
            native: Some(NativeExtensionManifest {
                library: None,
                targets: vec![current_target_triple().to_string()],
            }),
            wasm: None,
            web: None,
            build: Some(ExtensionBuildManifest {
                native: Some(NativeBuildManifest::Cargo {
                    manifest: "rust/Cargo.toml".to_string(),
                    package: None,
                }),
                wasm: None,
            }),
            module_owner: Some(
                vo_module::identity::ModulePath::parse("github.com/acme/demo").unwrap(),
            ),
            manifest_path: module_dir.join("vo.mod"),
        }
    }

    #[test]
    fn opaque_native_root_rejects_language_protocol_files_in_source_inputs() {
        for (label, relative) in [
            ("source", "native/helper.vo"),
            ("manifest-alias", "native/deep/VO.MOD"),
            ("lock-alias", "native/deep/Vo.LoCk"),
            ("work-alias", "native/deep/VO.WORK"),
        ] {
            let root = temp_dir(&format!("vo_native_opaque_protocol_{label}"));
            let module_dir = root.join("module");
            let opaque_root = module_dir.join("native");
            let forbidden = module_dir.join(relative);
            fs::create_dir_all(forbidden.parent().expect("forbidden parent")).unwrap();
            fs::write(&forbidden, b"reserved language input\n").unwrap();

            let mut walk = NativeInputWalkState::default();
            let mut paths = Vec::new();
            let error = collect_native_build_input_paths_with_opaque_root(
                &module_dir,
                &mut walk,
                &mut paths,
                NativeInputTreeKind::ModuleTree,
                &module_dir.join("cargo-output"),
                Some(&opaque_root),
            )
            .expect_err("opaque native root must reject language protocol files");
            assert_eq!(error.kind, ModuleSystemErrorKind::ValidationFailed);
            assert!(error.detail.contains("forbidden Vo source"), "{error}");
            assert!(
                error.detail.contains(&forbidden.display().to_string()),
                "{error}"
            );

            fs::remove_dir_all(root).unwrap();
        }
    }

    #[test]
    fn opaque_native_root_keeps_generated_and_tagged_cache_subtrees_opaque() {
        let root = temp_dir("vo_native_opaque_cache_boundaries");
        let module_dir = root.join("module");
        let opaque_root = module_dir.join("native");
        let target = opaque_root.join("Target");
        let tagged_cache = opaque_root.join("vendor-cache");
        fs::create_dir_all(target.join("deep")).unwrap();
        fs::create_dir_all(tagged_cache.join("deep")).unwrap();
        fs::write(target.join("deep/generated.vo"), b"generated\n").unwrap();
        fs::write(tagged_cache.join("deep/generated.vo"), b"cached\n").unwrap();
        fs::write(
            tagged_cache.join("CACHEDIR.TAG"),
            b"Signature: 8a477f597d28d172789f06886806bc55\n",
        )
        .unwrap();
        fs::write(opaque_root.join("lib.rs"), b"pub fn value() {}\n").unwrap();

        let mut walk = NativeInputWalkState::default();
        let mut paths = Vec::new();
        collect_native_build_input_paths_with_opaque_root(
            &module_dir,
            &mut walk,
            &mut paths,
            NativeInputTreeKind::ModuleTree,
            &module_dir.join("cargo-output"),
            Some(&opaque_root),
        )
        .expect("generated and declared cache boundaries remain opaque");

        assert!(paths.contains(&opaque_root.join("lib.rs")));
        assert!(!paths.iter().any(|path| path.starts_with(&target)));
        assert!(!paths.iter().any(|path| path.starts_with(&tagged_cache)));
        assert_eq!(
            super::super::host_input::directory_enumeration_count(&target),
            0
        );
        assert_eq!(
            super::super::host_input::directory_enumeration_count(&tagged_cache),
            0,
        );

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn native_input_exclusions_use_portable_aliases() {
        let target_directory = Path::new("module/cargo-output");
        for path in [
            "module/.GIT",
            "module/.VOLANG",
            "module/.VO-CACHE",
            "module/Target",
        ] {
            assert!(should_skip_native_build_input_dir(
                Path::new(path),
                NativeInputTreeKind::CargoPackage,
                target_directory,
            )
            .expect("portable native directory exclusion"));
        }
        assert!(should_skip_native_build_input_dir(
            Path::new("MODULE/Cargo-Output"),
            NativeInputTreeKind::CargoPackage,
            target_directory,
        )
        .expect("portable effective target-directory exclusion"));
        assert!(should_skip_native_build_input_dir(
            Path::new("module/NODE_MODULES"),
            NativeInputTreeKind::ModuleTree,
            target_directory,
        )
        .expect("portable module cache exclusion"));
        assert!(!should_skip_native_build_input_dir(
            Path::new("module/NODE_MODULES"),
            NativeInputTreeKind::CargoPackage,
            target_directory,
        )
        .expect("Cargo packages retain node_modules inputs"));

        for path in [
            "module/LIB.VOABI-TEST",
            "module/LIB.VO-ABI",
            "module/LIB.VO-INPUTS",
            "module/.VO-BUILD-LOCK-1",
        ] {
            assert!(should_skip_native_build_output_file(Path::new(path))
                .expect("portable native output exclusion"));
        }
    }

    #[test]
    fn local_extension_owner_is_retained_from_the_parsed_manifest_generation() {
        let root = temp_dir("vo_native_frozen_owner");
        fs::create_dir_all(&root).unwrap();
        let manifest_path = root.join("vo.mod");
        let manifest_text = |owner: &str| {
            format!(
                "module = \"{owner}\"\nvo = \"^0.1.0\"\n\n[extension]\nname = \"demo\"\n\n[extension.native]\ntargets = [\"{}\"]\n\n[build.native]\nkind = \"cargo\"\nmanifest = \"rust/Cargo.toml\"\n",
                current_target_triple(),
            )
        };
        let captured_text = manifest_text("github.com/acme/captured");
        fs::write(&manifest_path, &captured_text).unwrap();
        let manifest = parse_ext_manifest_content(&captured_text, &manifest_path).unwrap();

        fs::write(&manifest_path, manifest_text("github.com/acme/live-drift")).unwrap();

        assert_eq!(
            local_extension_module_owner(&manifest).unwrap(),
            "github.com/acme/captured",
        );
        fs::remove_dir_all(root).unwrap();
    }

    fn expected_test_native_path(module_dir: &Path) -> PathBuf {
        let profile = if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        };
        module_dir
            .join("rust/target")
            .join(current_target_triple())
            .join(profile)
            .join(current_platform_library_name("libdemo"))
    }

    fn write_native_input_fixture(module_dir: &Path) {
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
        fs::write(rust_dir.join("build.rs"), b"fn main() {}\n").unwrap();
        fs::write(rust_dir.join("src/lib.rs"), b"pub fn value() -> u8 { 1 }\n").unwrap();
        fs::write(rust_dir.join("build-input.bin"), [0, 1, 2, 255]).unwrap();
        generate_fixture_cargo_lock(&rust_dir.join("Cargo.toml"));
    }

    fn write_local_manifest(module_dir: &Path) {
        fs::write(
            module_dir.join("vo.mod"),
            format!(
                concat!(
                    "module = \"github.com/example/demo\"\n",
                    "vo = \"^0.1.0\"\n\n",
                    "[extension]\n",
                    "name = \"demo\"\n\n",
                    "[extension.native]\n",
                    "targets = [\"{}\"]\n\n",
                    "[build.native]\n",
                    "kind = \"cargo\"\n",
                    "manifest = \"rust/Cargo.toml\"\n",
                ),
                current_target_triple(),
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
        expected_test_native_path(module_dir)
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
        let (cargo_manifest, package) = configured_native_cargo_build(module_dir)?;
        let cargo = inspect_native_cargo_context(
            module_dir,
            &cargo_manifest,
            package.as_deref(),
            workspace_discovery,
        )?;
        compute_native_extension_input_fingerprint(
            module_dir,
            &cargo.manifest_path,
            &cargo.target_directory,
            workspace_discovery,
            || Ok("test-build-context".to_string()),
        )
    }

    fn capture_test_native_input_state(
        module_dir: &Path,
        workspace_discovery: &vo_module::workspace::WorkspaceDiscovery,
    ) -> Result<NativeExtensionInputState, ModuleSystemError> {
        let (cargo_manifest, package) = configured_native_cargo_build(module_dir)?;
        capture_native_extension_input_state(
            module_dir,
            &cargo_manifest,
            package.as_deref(),
            workspace_discovery,
        )
    }

    fn prime_native_extension_input_cache(module_dir: &Path, fingerprint: &str) -> PathBuf {
        let native_path = expected_test_native_path(module_dir);
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
        write_abi_native_extension_fixture(&module_dir);

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
        assert_eq!(specs[0].module_owner, "github.com/acme/demo");

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

    #[cfg(unix)]
    #[test]
    fn native_build_context_rejects_symlinked_cargo_configuration() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("vo_native_context_symlink");
        let module_dir = root.join("demo");
        write_native_input_fixture(&module_dir);
        let cargo_manifest = module_dir.join("rust/Cargo.toml");
        let cargo_config_dir = root.join(".cargo");
        let cargo_config_target = root.join("cargo-config-target.toml");
        fs::create_dir_all(&cargo_config_dir).unwrap();
        fs::write(&cargo_config_target, b"[net]\noffline = true\n").unwrap();
        symlink(&cargo_config_target, cargo_config_dir.join("config.toml")).unwrap();

        let error = native_build_context_fingerprint_for_manifest(&module_dir, &cargo_manifest)
            .unwrap_err();
        assert!(error.to_string().contains("symbolic links"), "{error}");

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
        let manifest_path = module_dir.join("vo.mod");
        let mut manifest = fs::read(&manifest_path).unwrap();
        manifest.extend_from_slice(
            b"\n[dependencies]\n\"github.com/example/dependency\" = \"^0.1.0\"\n",
        );
        fs::write(&manifest_path, manifest).unwrap();
        fs::create_dir_all(&dependency_dir).unwrap();
        fs::write(
            dependency_dir.join("vo.mod"),
            b"module = \"github.com/example/dependency\"\nvo = \"^0.1.0\"\n",
        )
        .unwrap();
        fs::write(
            dependency_dir.join("types.vo"),
            b"package dependency\n\ntype Value int\n",
        )
        .unwrap();
        let workfile = workspace.join("vo.work");
        fs::write(&workfile, b"version = 1\nmembers = [\"dependency\"]\n").unwrap();
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
            b"version = 1\nmembers = [\"dependency\"]\n# changed\n",
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
        let cargo_manifest = rust_dir.join("Cargo.toml");
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
                &cargo_manifest,
                rust_dir,
                &forward,
                &module_forward,
                &vo_module::workspace::WorkspaceDiscovery::Auto,
                "context",
            ),
            hash_native_build_inputs(
                &cargo_manifest,
                rust_dir,
                &reverse,
                &module_reverse,
                &vo_module::workspace::WorkspaceDiscovery::Auto,
                "context",
            ),
        );
        assert_ne!(
            hash_native_build_inputs(
                &cargo_manifest,
                rust_dir,
                &forward,
                &module_forward,
                &vo_module::workspace::WorkspaceDiscovery::Auto,
                "context",
            ),
            hash_native_build_inputs(
                &cargo_manifest,
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
    fn command_stdout_reader_start_failure_is_structured_and_reaps_child() {
        let mut command = std::process::Command::new(std::env::current_exe().unwrap());
        command.arg("--list");

        let error = run_command_with_bounded_output_and_spawner(
            &mut command,
            MAX_TEXT_FILE_BYTES,
            |_reader, _max_bytes, stream| {
                Err(std::io::Error::new(
                    std::io::ErrorKind::ResourceBusy,
                    format!("injected {stream} reader start failure"),
                ))
            },
        )
        .unwrap_err();

        assert_eq!(error.kind(), std::io::ErrorKind::ResourceBusy);
        assert!(error.to_string().contains("stdout reader"), "{error}");
        assert!(error.to_string().contains("injected"), "{error}");
    }

    #[test]
    fn command_stderr_reader_start_failure_joins_started_stdout_reader() {
        let mut command = std::process::Command::new(std::env::current_exe().unwrap());
        command.arg("--list");

        let error = run_command_with_bounded_output_and_spawner(
            &mut command,
            MAX_TEXT_FILE_BYTES,
            |reader, max_bytes, stream| {
                if stream == "stderr" {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::ResourceBusy,
                        "injected stderr reader start failure",
                    ));
                }
                spawn_bounded_command_output_reader(reader, max_bytes, stream)
            },
        )
        .unwrap_err();

        assert_eq!(error.kind(), std::io::ErrorKind::ResourceBusy);
        assert!(error.to_string().contains("stderr reader"), "{error}");
        assert!(error.to_string().contains("injected"), "{error}");
    }

    #[test]
    fn missing_cargo_diagnostic_explains_an_unset_path() {
        let error = std::io::Error::new(std::io::ErrorKind::NotFound, "missing executable");
        let detail = native_cargo_command_start_detail("locate-project", &error, false);

        assert!(detail.contains("`cargo locate-project`"), "{detail}");
        assert!(detail.contains("PATH is unset or empty"), "{detail}");
        assert!(detail.contains("native extension build"), "{detail}");
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
    fn explicit_missing_cargo_manifest_never_falls_back_to_a_prebuilt_artifact() {
        let root = temp_dir("vo_native_explicit_missing_cargo_manifest");
        let module_dir = root.join("module");
        fs::create_dir_all(&module_dir).unwrap();
        let native_path = module_dir
            .join("rust/target")
            .join(if cfg!(debug_assertions) {
                "debug"
            } else {
                "release"
            })
            .join(current_platform_library_name("libdemo"));
        fs::create_dir_all(native_path.parent().unwrap()).unwrap();
        fs::write(&native_path, b"stale prebuilt artifact").unwrap();
        let mut manifest = local_manifest(&module_dir);
        manifest.build.as_mut().unwrap().native = Some(NativeBuildManifest::Cargo {
            manifest: "rust/ext/Cargo.toml".to_string(),
            package: None,
        });
        let owner = local_extension_module_owner(&manifest).unwrap();

        let error = prepare_local_native_extension_load_path(
            &manifest,
            &module_dir,
            &owner,
            &root.join("mod-cache"),
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
            None,
        )
        .unwrap_err();
        assert_eq!(error.kind, ModuleSystemErrorKind::Missing);
        assert!(error
            .detail
            .contains("native extension Cargo manifest is missing"));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn local_native_extension_requires_a_declared_host_target() {
        let root = temp_dir("vo_native_missing_host_contract");
        let module_dir = root.join("module");
        let mod_root = root.join("mod-cache");
        fs::create_dir_all(&module_dir).unwrap();
        fs::create_dir_all(&mod_root).unwrap();
        write_local_manifest(&module_dir);
        let mut manifest = local_manifest(&module_dir);
        let other_target = if current_target_triple() == "aarch64-apple-darwin" {
            "x86_64-unknown-linux-gnu"
        } else {
            "aarch64-apple-darwin"
        };
        manifest.native.as_mut().unwrap().targets = vec![other_target.to_string()];

        let error = prepare_extension_spec(
            &manifest,
            &[],
            &mod_root,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
            None,
        )
        .unwrap_err();
        assert_eq!(error.kind, ModuleSystemErrorKind::Missing);
        assert!(error.detail.contains(current_target_triple()), "{error}");

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn explicit_prebuilt_path_never_probes_a_cargo_default() {
        let root = temp_dir("vo_native_explicit_prebuilt");
        let module_dir = root.join("module");
        fs::create_dir_all(&module_dir).unwrap();
        let native_path = module_dir
            .join("rust/target")
            .join(if cfg!(debug_assertions) {
                "debug"
            } else {
                "release"
            })
            .join(current_platform_library_name("libdemo"));
        fs::create_dir_all(native_path.parent().unwrap()).unwrap();
        fs::write(&native_path, b"prebuilt artifact").unwrap();
        let mut manifest = local_manifest(&module_dir);
        let relative = native_path.strip_prefix(&module_dir).unwrap();
        manifest.build.as_mut().unwrap().native = Some(NativeBuildManifest::Prebuilt {
            path: relative.to_string_lossy().replace('\\', "/"),
        });

        let configured = manifest_local_prebuilt_native_path(&manifest, &module_dir).unwrap();
        let selected = canonical_prebuilt_native_path(&module_dir, &configured).unwrap();
        assert_eq!(selected, native_path.canonicalize().unwrap());

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn prebuilt_preparation_authenticates_live_bytes_only_after_reachability() {
        let root = temp_dir("vo_native_reached_prebuilt");
        let module_dir = root.join("module");
        let native_path = write_abi_native_extension_fixture(&module_dir);
        let mut manifest = local_manifest(&module_dir);
        ensure_local_native_extension_built_with_workspace(
            &manifest,
            &module_dir,
            &root.join("mod-cache"),
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();
        let live_bytes = fs::read(&native_path).unwrap();
        let mut snapshot = super::super::snapshot::CompileInputSnapshot::default();
        snapshot
            .insert(native_path.clone(), b"stale base snapshot bytes".to_vec())
            .unwrap();
        let frozen_fs = super::super::snapshot::ResolverFs::snapshot_global(Arc::new(snapshot));

        let relative = native_path.strip_prefix(&module_dir).unwrap();
        manifest.build.as_mut().unwrap().native = Some(NativeBuildManifest::Prebuilt {
            path: relative.to_string_lossy().replace('\\', "/"),
        });
        let owner = local_extension_module_owner(&manifest).unwrap();

        let load_copy = prepare_local_native_extension_load_path(
            &manifest,
            &module_dir,
            &owner,
            &root.join("mod-cache"),
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
            Some(&frozen_fs),
        )
        .unwrap();

        assert_eq!(fs::read(load_copy.path).unwrap(), live_bytes);
        assert_eq!(fs::read(native_path).unwrap(), live_bytes);
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
            manifest_dir: rust_dir.clone(),
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
    fn cargo_target_directory_must_be_a_dedicated_output_subtree() {
        let module_dir = PathBuf::from("/workspace/module");
        let manifest_dir = module_dir.join("rust/ext");
        let workspace_root = module_dir.join("rust");
        let package_roots =
            BTreeSet::from([manifest_dir.clone(), PathBuf::from("/workspace/protocol")]);

        validate_native_cargo_target_directory_layout(
            &module_dir.join("rust/build-output"),
            &module_dir,
            &manifest_dir,
            &workspace_root,
            &package_roots,
        )
        .unwrap();

        for target_directory in [
            module_dir.clone(),
            workspace_root.clone(),
            module_dir.join("rust/ext"),
            PathBuf::from("/workspace"),
        ] {
            let error = validate_native_cargo_target_directory_layout(
                &target_directory,
                &module_dir,
                &manifest_dir,
                &workspace_root,
                &package_roots,
            )
            .unwrap_err();
            assert_eq!(error.kind, ModuleSystemErrorKind::ValidationFailed);
            assert!(error.detail.contains("dedicated generated-output"));
        }
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
        let cargo_manifest = rust_dir.join("Cargo.toml");

        let cargo = inspect_native_cargo_context(
            &module_dir,
            &cargo_manifest,
            None,
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
            &local_manifest(&module_dir),
            &module_dir,
            &root.join("mod-cache"),
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();
        assert_eq!(fs::read(root.join("Cargo.lock")).unwrap(), lock_before);
        assert!(!rust_dir.join("Cargo.lock").exists());

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn configured_cargo_manifest_builds_with_a_custom_named_target_directory() {
        let root = temp_dir("vo_native_virtual_cargo_workspace_member");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        let extension_dir = rust_dir.join("ext");
        let protocol_dir = rust_dir.join("protocol");
        write_module_source_fixture(&module_dir);
        fs::write(
            module_dir.join("vo.mod"),
            format!(
                concat!(
                    "module = \"github.com/example/demo\"\n",
                    "vo = \"^0.1.0\"\n\n",
                    "[extension]\n",
                    "name = \"demo\"\n\n",
                    "[extension.native]\n",
                    "targets = [\"{}\"]\n\n",
                    "[build.native]\n",
                    "kind = \"cargo\"\n",
                    "manifest = \"rust/Cargo.toml\"\n",
                    "package = \"demo\"\n",
                ),
                current_target_triple(),
            ),
        )
        .unwrap();
        fs::create_dir_all(extension_dir.join("src")).unwrap();
        fs::create_dir_all(protocol_dir.join("src")).unwrap();
        fs::create_dir_all(rust_dir.join(".cargo")).unwrap();
        fs::write(
            rust_dir.join(".cargo/config.toml"),
            "[build]\ntarget-dir = \"build-output\"\n",
        )
        .unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            "[workspace]\nresolver = \"2\"\nmembers = [\"ext\", \"protocol\"]\n",
        )
        .unwrap();
        fs::write(
            extension_dir.join("Cargo.toml"),
            concat!(
                "[package]\n",
                "name = \"demo\"\n",
                "version = \"0.1.0\"\n",
                "edition = \"2021\"\n\n",
                "[lib]\n",
                "crate-type = [\"cdylib\"]\n\n",
                "[dependencies]\n",
                "protocol = { path = \"../protocol\" }\n",
            ),
        )
        .unwrap();
        fs::write(
            extension_dir.join("src/lib.rs"),
            abi_native_extension_source(1),
        )
        .unwrap();
        fs::write(
            protocol_dir.join("Cargo.toml"),
            "[package]\nname = \"protocol\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        )
        .unwrap();
        fs::write(
            protocol_dir.join("src/lib.rs"),
            "pub const VERSION: u8 = 1;\n",
        )
        .unwrap();
        let cargo_manifest = rust_dir.join("Cargo.toml");
        generate_fixture_cargo_lock(&cargo_manifest);
        let lock_path = rust_dir.join("Cargo.lock");
        let lock_before = fs::read(&lock_path).unwrap();
        let native_path = rust_dir
            .join("build-output")
            .join(current_target_triple())
            .join(if cfg!(debug_assertions) {
                "debug"
            } else {
                "release"
            })
            .join(current_platform_library_name("libdemo"));

        let cargo = inspect_native_cargo_context(
            &module_dir,
            &cargo_manifest,
            Some("demo"),
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();
        assert_eq!(cargo.manifest_path, cargo_manifest.canonicalize().unwrap());
        assert_eq!(cargo.manifest_dir, rust_dir.canonicalize().unwrap());
        assert_eq!(cargo.workspace_root, rust_dir.canonicalize().unwrap());
        assert_eq!(cargo.lock_path, lock_path.canonicalize().unwrap());
        assert_eq!(
            cargo.target_directory,
            normalize_native_output_path(&rust_dir.join("build-output")).unwrap(),
        );
        assert_eq!(cargo.root_package_name, "demo");
        assert!(cargo
            .local_package_roots
            .contains(&protocol_dir.canonicalize().unwrap()));
        assert!(!extension_dir.join("Cargo.lock").exists());

        let discovery = vo_module::workspace::WorkspaceDiscovery::Disabled;
        let original_inputs = capture_native_extension_input_state(
            &module_dir,
            &cargo_manifest,
            Some("demo"),
            &discovery,
        )
        .unwrap();
        fs::write(
            protocol_dir.join("src/lib.rs"),
            "pub const VERSION: u8 = 2;\n",
        )
        .unwrap();
        let changed_inputs = capture_native_extension_input_state(
            &module_dir,
            &cargo_manifest,
            Some("demo"),
            &discovery,
        )
        .unwrap();
        assert_ne!(original_inputs.fingerprint, changed_inputs.fingerprint);

        let content = fs::read_to_string(module_dir.join("vo.mod")).unwrap();
        let extension_manifest =
            parse_ext_manifest_content(&content, &module_dir.join("vo.mod")).unwrap();
        let load_copy = ensure_local_native_extension_built_with_workspace(
            &extension_manifest,
            &module_dir,
            &root.join("mod-cache"),
            &discovery,
        )
        .unwrap();
        assert!(native_path.is_file());
        assert!(load_copy.path.is_file());
        assert_eq!(fs::read(&lock_path).unwrap(), lock_before);
        assert!(!extension_dir.join("Cargo.lock").exists());

        fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn configured_cargo_manifest_rejects_links_non_files_and_canonical_escape() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("vo_native_cargo_manifest_path_safety");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        let outside_dir = root.join("outside");
        fs::create_dir_all(&rust_dir).unwrap();
        fs::create_dir_all(&outside_dir).unwrap();
        let outside_manifest = outside_dir.join("Cargo.toml");
        fs::write(
            &outside_manifest,
            "[package]\nname = \"outside\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();

        let direct_link = rust_dir.join("Cargo.toml");
        symlink(&outside_manifest, &direct_link).unwrap();
        let link_error = canonical_native_cargo_manifest(&module_dir, &direct_link).unwrap_err();
        assert_eq!(link_error.kind, ModuleSystemErrorKind::ValidationFailed);
        assert!(link_error.detail.contains("symbolic links"));
        fs::remove_file(&direct_link).unwrap();

        fs::create_dir(&direct_link).unwrap();
        let directory_error =
            canonical_native_cargo_manifest(&module_dir, &direct_link).unwrap_err();
        assert_eq!(
            directory_error.kind,
            ModuleSystemErrorKind::ValidationFailed
        );
        assert!(directory_error.detail.contains("regular file"));
        fs::remove_dir(&direct_link).unwrap();

        let linked_dir = rust_dir.join("ext");
        symlink(&outside_dir, &linked_dir).unwrap();
        let escaped_manifest = linked_dir.join("Cargo.toml");
        let escape_error =
            canonical_native_cargo_manifest(&module_dir, &escaped_manifest).unwrap_err();
        assert_eq!(escape_error.kind, ModuleSystemErrorKind::ValidationFailed);
        assert!(escape_error.detail.contains("symbolic links"));
        fs::remove_file(&linked_dir).unwrap();

        let in_module_target = module_dir.join("real-ext");
        fs::create_dir(&in_module_target).unwrap();
        fs::write(
            in_module_target.join("Cargo.toml"),
            "[package]\nname = \"inside\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        symlink(&in_module_target, &linked_dir).unwrap();
        let alias_error =
            canonical_native_cargo_manifest(&module_dir, &escaped_manifest).unwrap_err();
        assert_eq!(alias_error.kind, ModuleSystemErrorKind::ValidationFailed);
        assert!(alias_error.detail.contains("symbolic links"));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn compiler_artifact_path_comes_from_cargo_machine_output() {
        let root = temp_dir("vo_native_artifact_binding");
        let rust_dir = root.join("rust");
        let actual = rust_dir
            .join("other-target/debug")
            .join(current_platform_library_name("libdemo"));
        fs::create_dir_all(actual.parent().unwrap()).unwrap();
        fs::write(&actual, b"new cargo artifact").unwrap();
        let package_id = "path+file:///tmp/demo#0.1.0";
        let cargo = NativeCargoContext {
            manifest_dir: rust_dir.clone(),
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
        let artifact_output = |path: &Path| {
            serde_json::to_vec(&serde_json::json!({
                "reason": "compiler-artifact",
                "package_id": package_id,
                "target": {
                    "name": "demo",
                    "kind": ["cdylib"],
                    "crate_types": ["cdylib"],
                },
                "filenames": [path],
                "fresh": false,
            }))
            .unwrap()
        };

        let output = artifact_output(&actual);
        let artifact = parse_built_cargo_artifact(&output, &cargo).unwrap();
        assert_eq!(artifact.path, actual.canonicalize().unwrap());

        let outside = rust_dir
            .join("outside")
            .join(current_platform_library_name("libdemo"));
        fs::create_dir_all(outside.parent().unwrap()).unwrap();
        fs::write(&outside, b"escaped cargo artifact").unwrap();
        let error = parse_built_cargo_artifact(&artifact_output(&outside), &cargo).unwrap_err();
        assert_eq!(
            error.kind,
            ModuleSystemErrorKind::BuildFailed,
            "{}",
            error.detail,
        );
        assert!(error.detail.contains("outside its target directory"));

        #[cfg(unix)]
        {
            use std::os::unix::fs::symlink;

            let linked = actual.with_file_name(current_platform_library_name("liblinked"));
            symlink(&actual, &linked).unwrap();
            let error = parse_built_cargo_artifact(&artifact_output(&linked), &cargo).unwrap_err();
            assert_eq!(error.kind, ModuleSystemErrorKind::ValidationFailed);
            assert!(error.detail.contains("following links"), "{}", error.detail);
        }

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn inactive_optional_cargo_path_dependency_is_not_a_build_input() {
        let root = temp_dir("vo_native_inactive_optional_path_dep");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        let optional_dir = rust_dir.join("optional");
        write_module_source_fixture(&module_dir);
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::create_dir_all(optional_dir.join("src")).unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            concat!(
                "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n",
                "[lib]\ncrate-type = [\"cdylib\"]\n\n",
                "[dependencies]\noptional = { path = \"optional\", optional = true }\n\n",
                "[workspace]\nmembers = [\"optional\"]\n",
            ),
        )
        .unwrap();
        fs::write(
            rust_dir.join("src/lib.rs"),
            "#[no_mangle]\npub extern \"C\" fn demo_value() -> u64 { 1 }\n",
        )
        .unwrap();
        fs::write(
            optional_dir.join("Cargo.toml"),
            "[package]\nname = \"optional\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        )
        .unwrap();
        fs::write(optional_dir.join("src/lib.rs"), "pub fn unused() {}\n").unwrap();
        generate_fixture_cargo_lock(&rust_dir.join("Cargo.toml"));

        let context = inspect_native_cargo_context(
            &module_dir,
            &rust_dir.join("Cargo.toml"),
            None,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();

        assert_eq!(
            context.local_package_roots,
            vec![rust_dir.canonicalize().unwrap()],
        );
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn inactive_platform_cargo_path_dependency_is_not_a_build_input() {
        let root = temp_dir("vo_native_inactive_platform_path_dep");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        let inactive_dir = root.join("inactive");
        write_module_source_fixture(&module_dir);
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::create_dir_all(inactive_dir.join("src")).unwrap();
        let inactive_target_os = if cfg!(target_os = "windows") {
            "linux"
        } else {
            "windows"
        };
        fs::write(
            rust_dir.join("Cargo.toml"),
            format!(
                concat!(
                    "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n",
                    "[lib]\ncrate-type = [\"cdylib\"]\n\n",
                    "[target.'cfg(target_os = \"{}\")'.dependencies]\n",
                    "inactive = {{ path = \"../../inactive\" }}\n",
                ),
                inactive_target_os,
            ),
        )
        .unwrap();
        fs::write(
            rust_dir.join("src/lib.rs"),
            "#[no_mangle]\npub extern \"C\" fn demo_value() -> u64 { 1 }\n",
        )
        .unwrap();
        fs::write(
            inactive_dir.join("Cargo.toml"),
            "[package]\nname = \"inactive\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        )
        .unwrap();
        fs::write(inactive_dir.join("src/lib.rs"), "pub fn unused() {}\n").unwrap();
        generate_fixture_cargo_lock(&rust_dir.join("Cargo.toml"));

        let context = inspect_native_cargo_context(
            &module_dir,
            &rust_dir.join("Cargo.toml"),
            None,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap();

        assert_eq!(
            context.local_package_roots,
            vec![rust_dir.canonicalize().unwrap()],
        );
        fs::remove_dir_all(root).unwrap();
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
        let discovery = vo_module::workspace::WorkspaceDiscovery::Disabled;

        let initial = capture_test_native_input_state(&module_dir, &discovery).unwrap();
        build_native_extension(&initial, &discovery).unwrap();
        let first = capture_test_native_input_state(&module_dir, &discovery).unwrap();
        let first_artifact = build_native_extension(&first, &discovery).unwrap();
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

        let second = capture_test_native_input_state(&module_dir, &discovery).unwrap();
        assert_ne!(first.fingerprint, second.fingerprint);
        assert_ne!(first.build_token, second.build_token);
        let second_artifact = build_native_extension(&second, &discovery).unwrap();
        let second_bytes = fs::read(&second_artifact.path).unwrap();
        assert!(!second_artifact.fresh);
        assert_ne!(first_bytes, second_bytes);

        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn cargo_path_dependency_rejects_symlinked_source_roots() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("vo_native_path_dep_symlink");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        let dependency_dir = root.join("real-dep");
        let dependency_link = root.join("dep-link");
        write_module_source_fixture(&module_dir);
        fs::create_dir_all(rust_dir.join("src")).unwrap();
        fs::create_dir_all(dependency_dir.join("src")).unwrap();
        fs::write(
            rust_dir.join("Cargo.toml"),
            concat!(
                "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n",
                "[lib]\ncrate-type = [\"cdylib\"]\n\n",
                "[dependencies]\ndep = { path = \"../../dep-link\" }\n",
            ),
        )
        .unwrap();
        fs::write(
            rust_dir.join("src/lib.rs"),
            "pub fn value() { dep::value(); }\n",
        )
        .unwrap();
        fs::write(
            dependency_dir.join("Cargo.toml"),
            "[package]\nname = \"dep\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        )
        .unwrap();
        fs::write(dependency_dir.join("src/lib.rs"), "pub fn value() {}\n").unwrap();
        symlink(&dependency_dir, &dependency_link).unwrap();
        generate_fixture_cargo_lock(&rust_dir.join("Cargo.toml"));

        let error = inspect_native_cargo_context(
            &module_dir,
            &rust_dir.join("Cargo.toml"),
            None,
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap_err();

        assert_eq!(
            error.kind,
            ModuleSystemErrorKind::ValidationFailed,
            "{error}"
        );
        assert!(error.detail.contains("without following links"), "{error}");
        fs::remove_dir_all(root).unwrap();
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
        let discovery = vo_module::workspace::WorkspaceDiscovery::Disabled;

        let initial = capture_test_native_input_state(&module_dir, &discovery).unwrap();
        build_native_extension(&initial, &discovery).unwrap();
        let first = capture_test_native_input_state(&module_dir, &discovery).unwrap();
        let first_artifact = build_native_extension(&first, &discovery).unwrap();
        let first_bytes = fs::read(&first_artifact.path).unwrap();
        let future = SystemTime::now() + std::time::Duration::from_secs(2);
        fs::write(&external, b"2\n").unwrap();
        fs::OpenOptions::new()
            .write(true)
            .open(&external)
            .unwrap()
            .set_times(fs::FileTimes::new().set_modified(future))
            .unwrap();

        let second = capture_test_native_input_state(&module_dir, &discovery).unwrap();
        assert_eq!(first.fingerprint, second.fingerprint);
        assert_eq!(first.build_token, second.build_token);
        let second_artifact = build_native_extension(&second, &discovery).unwrap();
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
        write_simple_cdylib_fixture(
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
            &local_manifest(&module_dir),
            &module_dir,
            &root.join("mod-cache"),
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        )
        .unwrap_err();
        assert_eq!(
            error.kind,
            ModuleSystemErrorKind::BuildFailed,
            "{}",
            error.detail,
        );
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
        let lock_bytes =
            b"version = 4\n\n[[package]]\nname = \"vo-stdlib-source\"\nversion = \"0.1.1\"\n";
        assert!(
            local_volang_cargo_patch_configs(lock_bytes, Path::new("Cargo.lock"))
                .unwrap()
                .iter()
                .any(|config| config.contains("vo-stdlib-source"))
        );
    }

    #[test]
    fn local_volang_patch_selection_preserves_recorded_unused_patches_only() {
        let lock_bytes = br#"version = 4

[[package]]
name = "vo-runtime"
version = "0.1.1"

[[patch.unused]]
name = "vo-web"
version = "0.1.1"

[metadata]
name = "vo-engine"
"#;
        let packages = vec![
            ("vo-runtime", PathBuf::from("/local/vo-runtime")),
            ("vo-web", PathBuf::from("/local/vo-web")),
            ("vo-engine", PathBuf::from("/local/vo-engine")),
        ];
        let configs = cargo_patch_configs_for_locked_packages(
            &["https://github.com/vo-lang/volang"],
            &packages,
            lock_bytes,
            Path::new("Cargo.lock"),
        )
        .unwrap();

        assert_eq!(configs.len(), 2);
        assert!(configs.iter().any(|config| config.contains("vo-runtime")));
        assert!(configs.iter().any(|config| config.contains("vo-web")));
        assert!(!configs.iter().any(|config| config.contains("vo-engine")));
    }

    #[test]
    fn locked_cargo_patch_selection_is_stable_in_engine_and_plain_contexts() {
        let root = temp_dir("vo_native_locked_patch_contexts");
        let source_repo = root.join("source");
        let patched_used = root.join("patched-used");
        let patched_unused = root.join("patched-unused");
        let patched_unrecorded = root.join("patched-unrecorded");
        let consumer = root.join("consumer");
        let cargo_home = root.join("cargo-home");
        for package_dir in [
            &source_repo,
            &patched_used,
            &patched_unused,
            &patched_unrecorded,
        ] {
            fs::create_dir_all(package_dir.join("src")).unwrap();
        }
        fs::create_dir_all(consumer.join("src")).unwrap();
        fs::create_dir_all(&cargo_home).unwrap();
        fs::write(
            source_repo.join("Cargo.toml"),
            "[package]\nname = \"used\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        )
        .unwrap();
        fs::write(
            source_repo.join("src/lib.rs"),
            "pub fn value() -> u8 { 1 }\n",
        )
        .unwrap();
        for (package_dir, name) in [
            (&patched_used, "used"),
            (&patched_unused, "unused"),
            (&patched_unrecorded, "unrecorded"),
        ] {
            fs::write(
                package_dir.join("Cargo.toml"),
                format!("[package]\nname = {name:?}\nversion = \"0.1.0\"\nedition = \"2021\"\n",),
            )
            .unwrap();
            fs::write(
                package_dir.join("src/lib.rs"),
                "pub fn value() -> u8 { 2 }\n",
            )
            .unwrap();
        }
        let git = |arguments: &[&str]| {
            let output = std::process::Command::new("git")
                .args(arguments)
                .current_dir(&source_repo)
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "git {:?} failed: {}",
                arguments,
                String::from_utf8_lossy(&output.stderr),
            );
        };
        git(&["init", "--quiet"]);
        git(&["add", "Cargo.toml", "src/lib.rs"]);
        git(&[
            "-c",
            "user.name=Volang Test",
            "-c",
            "user.email=volang-test@example.invalid",
            "commit",
            "--quiet",
            "-m",
            "fixture",
        ]);

        let source_url = if cfg!(windows) {
            format!(
                "file:///{}",
                source_repo.to_string_lossy().replace('\\', "/")
            )
        } else {
            format!("file://{}", source_repo.display())
        };
        fs::write(
            consumer.join("Cargo.toml"),
            format!(
                concat!(
                    "[package]\n",
                    "name = \"consumer\"\n",
                    "version = \"0.1.0\"\n",
                    "edition = \"2021\"\n\n",
                    "[dependencies]\n",
                    "used = {{ git = {} }}\n\n",
                    "[patch.{}]\n",
                    "used = {{ path = {} }}\n",
                    "unused = {{ path = {} }}\n",
                ),
                toml_quote(&source_url),
                toml_quote(&source_url),
                toml_quote(&patched_used.to_string_lossy()),
                toml_quote(&patched_unused.to_string_lossy()),
            ),
        )
        .unwrap();
        fs::write(
            consumer.join("src/lib.rs"),
            "pub fn value() -> u8 { used::value() }\n",
        )
        .unwrap();

        let manifest_path = consumer.join("Cargo.toml");
        let run_cargo = |arguments: &[&str], configs: &[String]| {
            let mut command = std::process::Command::new("cargo");
            command
                .args(arguments)
                .arg("--manifest-path")
                .arg(&manifest_path);
            for config in configs {
                command.arg("--config").arg(config);
            }
            command
                .current_dir(&consumer)
                .env("CARGO_HOME", &cargo_home)
                .env_remove("VOWORK")
                .output()
                .unwrap()
        };
        let generated = run_cargo(&["generate-lockfile"], &[]);
        assert!(
            generated.status.success(),
            "failed to generate fixture lock: {}",
            String::from_utf8_lossy(&generated.stderr),
        );
        let lock_path = consumer.join("Cargo.lock");
        let lock_bytes = fs::read(&lock_path).unwrap();
        assert!(String::from_utf8_lossy(&lock_bytes).contains("[[patch.unused]]"));
        let packages = vec![
            ("used", patched_used.clone()),
            ("unused", patched_unused.clone()),
            ("unrecorded", patched_unrecorded.clone()),
        ];
        let configs = cargo_patch_configs_for_locked_packages(
            &[source_url.as_str()],
            &packages,
            &lock_bytes,
            &lock_path,
        )
        .unwrap();
        assert_eq!(configs.len(), 2);
        assert!(configs.iter().any(|config| config.contains("\"used\"")));
        assert!(configs.iter().any(|config| config.contains("\"unused\"")));
        assert!(!configs
            .iter()
            .any(|config| config.contains("\"unrecorded\"")));

        for arguments in [
            vec!["metadata", "--format-version", "1", "--locked", "--offline"],
            vec!["build", "--locked", "--offline"],
        ] {
            let output = run_cargo(&arguments, &configs);
            assert!(
                output.status.success(),
                "engine-style cargo {:?} failed: {}",
                arguments,
                String::from_utf8_lossy(&output.stderr),
            );
            assert_eq!(fs::read(&lock_path).unwrap(), lock_bytes);
        }
        for arguments in [
            vec!["metadata", "--format-version", "1", "--locked", "--offline"],
            vec!["check", "--locked", "--offline"],
        ] {
            let output = run_cargo(&arguments, &[]);
            assert!(
                output.status.success(),
                "plain cargo {:?} failed: {}",
                arguments,
                String::from_utf8_lossy(&output.stderr),
            );
            assert_eq!(fs::read(&lock_path).unwrap(), lock_bytes);
        }

        let unrecorded_config = format!(
            "patch.{}.{}.path={}",
            toml_quote(&source_url),
            toml_quote("unrecorded"),
            toml_quote(&patched_unrecorded.to_string_lossy()),
        );
        let mut polluted_configs = configs;
        polluted_configs.push(unrecorded_config);
        let polluted = run_cargo(
            &["metadata", "--format-version", "1", "--locked", "--offline"],
            &polluted_configs,
        );
        assert!(!polluted.status.success());
        assert_eq!(fs::read(&lock_path).unwrap(), lock_bytes);

        fs::remove_dir_all(root).unwrap();
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
        assert_eq!(error.kind, ModuleSystemErrorKind::ValidationFailed);

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
        assert_eq!(
            error.kind,
            ModuleSystemErrorKind::ValidationFailed,
            "{error:?}",
        );

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
        let target_directory = rust_dir.join("cargo-output");
        let depth_error = collect_native_build_inputs(&rust_dir, &target_directory).unwrap_err();
        assert_eq!(depth_error.kind, ModuleSystemErrorKind::ValidationFailed);
        fs::remove_dir_all(&rust_dir).unwrap();

        fs::create_dir_all(&rust_dir).unwrap();
        fs::write(rust_dir.join("a"), b"a").unwrap();
        fs::write(rust_dir.join("b"), b"b").unwrap();
        let mut walk = NativeInputWalkState::with_entry_limit(1);
        let entry_error = collect_native_build_input_paths(
            &rust_dir,
            &mut walk,
            &mut Vec::new(),
            NativeInputTreeKind::CargoPackage,
            &target_directory,
        )
        .unwrap_err();
        assert_eq!(entry_error.kind, ModuleSystemErrorKind::ValidationFailed);

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

        let error =
            collect_native_build_inputs(&rust_dir, &rust_dir.join("cargo-output")).unwrap_err();
        assert_eq!(error.kind, ModuleSystemErrorKind::ValidationFailed);

        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn cargo_package_input_walk_excludes_generated_caches_and_retains_node_modules() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("vo_native_walk_ignored_trees");
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("src/lib.rs"), b"source").unwrap();
        let target_directory = root.join("cargo-output");
        for ignored in ["cargo-output", ".git", ".volang", ".vo-cache", "target"] {
            let ignored_dir = root.join(ignored);
            fs::create_dir_all(&ignored_dir).unwrap();
            symlink(root.join("src/lib.rs"), ignored_dir.join("alias")).unwrap();
        }
        let node_modules = root.join("node_modules");
        fs::create_dir_all(&node_modules).unwrap();
        fs::write(node_modules.join("input.txt"), b"node_modules").unwrap();
        let tagged_cache = root.join("custom-cache");
        fs::create_dir_all(&tagged_cache).unwrap();
        fs::write(
            tagged_cache.join("CACHEDIR.TAG"),
            b"Signature: 8a477f597d28d172789f06886806bc55\n# generated\n",
        )
        .unwrap();
        symlink(root.join("src/lib.rs"), tagged_cache.join("alias")).unwrap();

        let inputs = collect_native_build_inputs(&root, &target_directory).unwrap();
        assert_eq!(
            inputs,
            vec![
                (
                    PathBuf::from("node_modules/input.txt"),
                    b"node_modules".to_vec(),
                ),
                (PathBuf::from("src/lib.rs"), b"source".to_vec()),
            ]
        );

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn cargo_path_dependency_cache_churn_is_excluded_from_native_inputs() {
        let root = temp_dir("vo_native_path_dep_cache_churn");
        let dependency = root.join("path-dependency");
        let target = dependency.join("target/debug/deps");
        let tagged_cache = dependency.join("custom-cache");
        fs::create_dir_all(dependency.join("src")).unwrap();
        fs::create_dir_all(&target).unwrap();
        fs::create_dir_all(&tagged_cache).unwrap();
        fs::write(
            dependency.join("Cargo.toml"),
            "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(dependency.join("src/lib.rs"), "pub fn value() {}\n").unwrap();
        fs::write(
            tagged_cache.join("CACHEDIR.TAG"),
            b"Signature: 8a477f597d28d172789f06886806bc55\r\n# generated\r\n",
        )
        .unwrap();

        let running = Arc::new(AtomicBool::new(true));
        let writer_running = Arc::clone(&running);
        let writer_target = target.clone();
        let writer_cache = tagged_cache.clone();
        let writer = std::thread::spawn(move || {
            let mut generation = 0usize;
            while writer_running.load(AtomicOrdering::Acquire) {
                generation = generation.wrapping_add(1);
                fs::write(
                    writer_target.join("libdep.rmeta"),
                    vec![b'x'; generation % 257],
                )
                .unwrap();
                let volatile = writer_cache.join(format!("entry-{}", generation % 2));
                fs::write(&volatile, generation.to_le_bytes()).unwrap();
                let _ = fs::remove_file(
                    writer_cache.join(format!("entry-{}", generation.wrapping_add(1) % 2)),
                );
                std::thread::yield_now();
            }
        });

        let captures = (0..8)
            .map(|_| collect_native_build_inputs(&dependency, &root.join("engine-target")))
            .collect::<Vec<_>>();
        running.store(false, AtomicOrdering::Release);
        writer.join().unwrap();

        for inputs in captures {
            let inputs =
                inputs.expect("generated cache churn must not perturb path-dependency capture");
            assert_eq!(
                inputs,
                vec![
                    (
                        PathBuf::from("Cargo.toml"),
                        b"[package]\nname = \"dep\"\nversion = \"0.1.0\"\n".to_vec(),
                    ),
                    (PathBuf::from("src/lib.rs"), b"pub fn value() {}\n".to_vec(),),
                ],
            );
        }

        fs::remove_dir_all(root).unwrap();
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

    #[test]
    fn local_patch_input_walk_uses_portable_exclusion_keys() {
        let root = temp_dir("vo_patch_walk_portable_exclusions");
        fs::create_dir_all(&root).unwrap();
        for excluded in [".GIT", "Target", "NODE_MODULES"] {
            let directory = root.join(excluded);
            fs::create_dir_all(&directory).unwrap();
            fs::write(directory.join("ignored"), b"ignored").unwrap();
        }
        fs::write(root.join("included"), b"included").unwrap();
        let mut files = BTreeSet::new();
        let mut walk = NativeInputWalkState::default();

        collect_local_patch_input_files(&root, &mut files, &mut walk).unwrap();

        assert_eq!(
            files,
            BTreeSet::from([root.join("included").canonicalize().unwrap()])
        );
        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn local_patch_input_walk_rejects_non_utf8_directory_names() {
        use std::os::unix::ffi::OsStringExt as _;

        let root = temp_dir("vo_patch_walk_non_utf8");
        fs::create_dir_all(&root).unwrap();
        let directory = root.join(std::ffi::OsString::from_vec(b"bad-\xff".to_vec()));
        match fs::create_dir(&directory) {
            Ok(()) => {
                let mut files = BTreeSet::new();
                let mut walk = NativeInputWalkState::default();
                let error =
                    collect_local_patch_input_files(&root, &mut files, &mut walk).unwrap_err();
                assert_eq!(error.kind(), std::io::ErrorKind::InvalidData);
            }
            Err(error) => {
                // Some Unix hosts reject non-UTF-8 path components before the
                // walker can observe them. Only that exact failure is valid.
                assert_eq!(
                    error.raw_os_error(),
                    Some(libc::EILSEQ),
                    "unexpected mkdir failure: {error}"
                );
            }
        }
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
        let native_path = expected_test_native_path(&module_dir);
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
    fn atomic_native_file_replacement_never_exposes_a_missing_destination() {
        let root = temp_dir("vo_native_atomic_replace_visibility");
        fs::create_dir_all(&root).unwrap();
        let destination = Arc::new(root.join("marker"));
        fs::write(&*destination, b"initial").unwrap();
        let running = Arc::new(AtomicBool::new(true));
        let observed_missing = Arc::new(AtomicBool::new(false));
        let barrier = Arc::new(Barrier::new(2));
        let reader_destination = Arc::clone(&destination);
        let reader_running = Arc::clone(&running);
        let reader_missing = Arc::clone(&observed_missing);
        let reader_barrier = Arc::clone(&barrier);
        let reader = std::thread::spawn(move || {
            reader_barrier.wait();
            while reader_running.load(AtomicOrdering::Acquire) {
                if fs::symlink_metadata(&*reader_destination)
                    .is_err_and(|error| error.kind() == std::io::ErrorKind::NotFound)
                {
                    reader_missing.store(true, AtomicOrdering::Release);
                    break;
                }
                std::thread::yield_now();
            }
        });

        barrier.wait();
        for generation in 0..512u32 {
            atomic_write_native_file(&destination, &generation.to_le_bytes()).unwrap();
            std::thread::yield_now();
        }
        running.store(false, AtomicOrdering::Release);
        reader.join().unwrap();

        assert!(!observed_missing.load(AtomicOrdering::Acquire));
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn load_copy_recovers_an_exact_artifact_missing_its_marker() {
        let root = temp_dir("vo_native_load_copy_marker_recovery");
        fs::create_dir_all(&root).unwrap();
        let native_path = root.join(current_platform_library_name("libdemo"));
        let bytes = b"exact native generation";
        let load_path = native_extension_load_copy_path_for_bytes(&native_path, bytes);
        fs::write(&load_path, bytes).unwrap();
        assert!(!native_extension_abi_marker_path(&load_path).exists());

        let load_copy = materialize_native_extension_load_copy_from_bytes(&native_path, bytes)
            .expect("an exact crash-left artifact must regain its marker");

        assert_eq!(load_copy.path, load_path);
        assert_eq!(
            fs::read_to_string(native_extension_abi_marker_path(&load_path)).unwrap(),
            native_extension_abi_marker_for_bytes(bytes),
        );
        drop(load_copy);
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn load_copy_refuses_to_replace_a_corrupt_content_addressed_generation() {
        let root = temp_dir("vo_native_load_copy_corrupt");
        fs::create_dir_all(&root).unwrap();
        let native_path = root.join(current_platform_library_name("libdemo"));
        let expected = b"expected native generation";
        let load_path = native_extension_load_copy_path_for_bytes(&native_path, expected);
        fs::write(&load_path, b"corrupt bytes").unwrap();

        let error =
            materialize_native_extension_load_copy_from_bytes(&native_path, expected).unwrap_err();

        assert!(error.detail.contains("refusing to replace"), "{error}");
        assert_eq!(fs::read(&load_path).unwrap(), b"corrupt bytes");
        assert!(!native_extension_abi_marker_path(&load_path).exists());
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn load_copy_gc_preserves_leases_and_collects_inactive_and_orphaned_metadata() {
        let root = temp_dir("vo_native_load_copy_gc");
        fs::create_dir_all(&root).unwrap();
        let native_path = root.join(current_platform_library_name("libdemo"));
        let first =
            materialize_native_extension_load_copy_from_bytes(&native_path, b"first").unwrap();
        let first_path = first.path.clone();
        let second =
            materialize_native_extension_load_copy_from_bytes(&native_path, b"second").unwrap();
        let second_path = second.path.clone();
        assert!(first_path.is_file());
        assert!(second_path.is_file());

        drop(first);
        drop(second);
        let current =
            materialize_native_extension_load_copy_from_bytes(&native_path, b"current").unwrap();
        assert!(!first_path.exists());
        assert!(!second_path.exists());

        let orphan =
            materialize_native_extension_load_copy_from_bytes(&native_path, b"orphan").unwrap();
        let orphan_path = orphan.path.clone();
        let orphan_marker = native_extension_abi_marker_path(&orphan_path);
        let orphan_lease = native_extension_load_copy_lease_path(&orphan_path);
        drop(orphan);
        fs::remove_file(&orphan_path).unwrap();
        assert!(orphan_marker.is_file());
        assert!(orphan_lease.is_file());

        let refreshed =
            materialize_native_extension_load_copy_from_bytes(&native_path, b"current").unwrap();
        assert_eq!(refreshed.path, current.path);
        assert!(!orphan_marker.exists());
        assert!(!orphan_lease.exists());

        drop(refreshed);
        drop(current);
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn concurrent_load_copy_materialization_converges_on_one_generation() {
        const WORKERS: usize = 8;
        let root = temp_dir("vo_native_load_copy_concurrent");
        fs::create_dir_all(&root).unwrap();
        let native_path = Arc::new(root.join(current_platform_library_name("libdemo")));
        let barrier = Arc::new(Barrier::new(WORKERS));
        let mut workers = Vec::new();
        for _ in 0..WORKERS {
            let native_path = Arc::clone(&native_path);
            let barrier = Arc::clone(&barrier);
            workers.push(std::thread::spawn(move || {
                barrier.wait();
                materialize_native_extension_load_copy_from_bytes(
                    &native_path,
                    b"shared generation",
                )
                .unwrap()
            }));
        }
        let copies = workers
            .into_iter()
            .map(|worker| worker.join().unwrap())
            .collect::<Vec<_>>();
        let expected =
            native_extension_load_copy_path_for_bytes(&native_path, b"shared generation");
        assert!(copies.iter().all(|copy| copy.path == expected));
        let (prefix, suffix) = native_extension_load_copy_family(&native_path);
        let generations = fs::read_dir(&root)
            .unwrap()
            .map(|entry| entry.unwrap().file_name())
            .filter_map(|name| name.to_str().map(str::to_owned))
            .filter(|name| native_extension_load_copy_name_matches_family(name, &prefix, &suffix))
            .count();
        assert_eq!(generations, 1);

        drop(copies);
        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn load_store_and_lease_locks_reject_symbolic_links() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("vo_native_load_lock_symlink");
        fs::create_dir_all(&root).unwrap();
        let native_path = root.join(current_platform_library_name("libdemo"));
        let external = root.join("external");
        fs::write(&external, b"external").unwrap();
        let store_lock = native_extension_load_store_lock_path(&native_path);
        symlink(&external, &store_lock).unwrap();
        assert!(acquire_native_extension_load_store_file_lock(&store_lock).is_err());
        fs::remove_file(&store_lock).unwrap();

        let load_path = native_extension_load_copy_path_for_bytes(&native_path, b"generation");
        let lease_path = native_extension_load_copy_lease_path(&load_path);
        symlink(&external, &lease_path).unwrap();
        assert!(open_native_extension_load_copy_lease_file(&load_path).is_err());

        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn replaced_load_store_and_lease_paths_fail_identity_revalidation() {
        let root = temp_dir("vo_native_load_lock_replaced");
        fs::create_dir_all(&root).unwrap();
        let native_path = root.join(current_platform_library_name("libdemo"));
        let store_path = native_extension_load_store_lock_path(&native_path);
        let store = acquire_native_extension_load_store_file_lock(&store_path).unwrap();
        fs::rename(&store_path, root.join("old-store-lock")).unwrap();
        fs::write(&store_path, b"replacement").unwrap();
        assert!(validate_native_lock_path_identity(&store, &store_path).is_err());

        let load_path = native_extension_load_copy_path_for_bytes(&native_path, b"generation");
        let lease_path = native_extension_load_copy_lease_path(&load_path);
        let lease = acquire_native_extension_load_copy_lease(&load_path).unwrap();
        fs::rename(&lease_path, root.join("old-lease")).unwrap();
        fs::write(&lease_path, b"replacement").unwrap();
        assert!(validate_native_lock_path_identity(&lease._file, &lease_path).is_err());

        drop(lease);
        drop(store);
        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(windows)]
    #[test]
    fn load_store_and_lease_locks_reject_reparse_points() {
        use std::os::windows::fs::symlink_file;

        let root = temp_dir("vo_native_load_lock_reparse");
        fs::create_dir_all(&root).unwrap();
        let native_path = root.join(current_platform_library_name("libdemo"));
        let external = root.join("external");
        fs::write(&external, b"external").unwrap();
        let store_lock = native_extension_load_store_lock_path(&native_path);
        if let Err(error) = symlink_file(&external, &store_lock) {
            if error.kind() == std::io::ErrorKind::PermissionDenied {
                fs::remove_dir_all(&root).unwrap();
                return;
            }
            panic!("failed to create reparse-point fixture: {error}");
        }
        assert!(acquire_native_extension_load_store_file_lock(&store_lock).is_err());
        fs::remove_file(&store_lock).unwrap();

        let load_path = native_extension_load_copy_path_for_bytes(&native_path, b"generation");
        let lease_path = native_extension_load_copy_lease_path(&load_path);
        symlink_file(&external, &lease_path).unwrap();
        assert!(open_native_extension_load_copy_lease_file(&load_path).is_err());

        fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(windows)]
    #[test]
    fn active_load_store_and_lease_handles_deny_path_replacement() {
        let root = temp_dir("vo_native_load_lock_replacement_denied");
        fs::create_dir_all(&root).unwrap();
        let native_path = root.join(current_platform_library_name("libdemo"));
        let store_path = native_extension_load_store_lock_path(&native_path);
        let store = acquire_native_extension_load_store_file_lock(&store_path).unwrap();
        assert!(fs::rename(&store_path, root.join("moved-store-lock")).is_err());

        let load_path = native_extension_load_copy_path_for_bytes(&native_path, b"generation");
        let lease_path = native_extension_load_copy_lease_path(&load_path);
        let lease = acquire_native_extension_load_copy_lease(&load_path).unwrap();
        assert!(fs::rename(&lease_path, root.join("moved-lease")).is_err());

        drop(lease);
        drop(store);
        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn cached_local_extension_checks_original_inputs_and_load_copy() {
        let root = temp_dir("vo_native_cached_spec");
        let module_dir = root.join("demo");
        let native_path = write_abi_native_extension_fixture(&module_dir);
        let workspace_discovery = vo_module::workspace::WorkspaceDiscovery::Disabled;
        let manifest = local_manifest(&module_dir);
        let mod_root = root.join("mod-cache");
        let load_copy = ensure_local_native_extension_built_with_workspace(
            &manifest,
            &module_dir,
            &mod_root,
            &workspace_discovery,
        )
        .unwrap();
        let load_path = load_copy.path.clone();
        drop(load_copy);
        let mut spec = NativeExtensionSpec::new(
            "demo",
            "github.com/example/demo",
            load_path.clone(),
            module_dir.join("vo.mod"),
        );

        let refreshed = ensure_local_native_extension_built_with_workspace(
            &manifest,
            &module_dir,
            &mod_root,
            &workspace_discovery,
        )
        .unwrap();
        assert!(
            paths_refer_to_same_file(&load_path, &refreshed.path),
            "{} != {}",
            load_path.display(),
            refreshed.path.display(),
        );
        drop(refreshed);
        assert!(spec
            .native_path
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name.contains(".voabi-")));
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
        let mut snapshot = super::super::snapshot::CompileInputSnapshot::default();
        snapshot
            .insert(
                spec.manifest_path.clone(),
                fs::read(&spec.manifest_path).unwrap(),
            )
            .unwrap();
        let frozen_input_fs =
            super::super::snapshot::ResolverFs::snapshot_global(Arc::new(snapshot));
        assert!(cached_native_extension_specs_match_frozen_inputs(
            std::slice::from_mut(&mut spec),
            &frozen_input_fs,
            &[],
            &mod_root,
            &workspace_discovery,
        ));
        let alternate = materialize_native_extension_load_copy_from_bytes(
            &native_path,
            b"cache lease probe generation",
        )
        .unwrap();
        assert!(
            load_path.is_file(),
            "a refreshed cached specification must retain its load-copy lease"
        );
        drop(alternate);
        atomic_write_native_file(
            &module_dir.join("rust/src/lib.rs"),
            abi_native_extension_source(2).as_bytes(),
        )
        .unwrap();
        assert!(!cached_native_extension_specs_match_frozen_inputs(
            std::slice::from_mut(&mut spec),
            &frozen_input_fs,
            &[],
            &mod_root,
            &workspace_discovery,
        ));
        atomic_write_native_file(
            &module_dir.join("rust/src/lib.rs"),
            abi_native_extension_source(1).as_bytes(),
        )
        .unwrap();
        let restored_matches_old_spec = cached_native_extension_specs_match_frozen_inputs(
            std::slice::from_mut(&mut spec),
            &frozen_input_fs,
            &[],
            &mod_root,
            &workspace_discovery,
        );
        let restored_load_copy = ensure_local_native_extension_built_with_workspace(
            &manifest,
            &module_dir,
            &mod_root,
            &workspace_discovery,
        )
        .unwrap();
        assert_eq!(
            restored_matches_old_spec,
            paths_refer_to_same_file(&spec.native_path, &restored_load_copy.path),
        );
        let mut restored_spec = native_extension_spec_with_load_copy(
            &manifest,
            "github.com/example/demo",
            restored_load_copy,
        );
        assert!(cached_native_extension_specs_match_frozen_inputs(
            std::slice::from_mut(&mut restored_spec),
            &frozen_input_fs,
            &[],
            &mod_root,
            &workspace_discovery,
        ));
        fs::create_dir_all(module_dir.join("rust/.cargo")).unwrap();
        fs::write(
            module_dir.join("rust/.cargo/config.toml"),
            "[build]\ntarget-dir = \"alternate-target\"\n",
        )
        .unwrap();
        assert!(!cached_native_extension_specs_match_frozen_inputs(
            std::slice::from_mut(&mut restored_spec),
            &frozen_input_fs,
            &[],
            &mod_root,
            &workspace_discovery,
        ));

        fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn frozen_cache_provenance_rejects_an_arbitrary_existing_native_path() {
        let root = temp_dir("vo_native_cached_arbitrary_path");
        let native_path = root.join(current_platform_library_name("libunrelated"));
        fs::create_dir_all(&root).unwrap();
        fs::write(&native_path, b"unrelated native bytes").unwrap();
        let mut spec = NativeExtensionSpec::new(
            "demo",
            "github.com/example/demo",
            native_path,
            root.join("unrelated/vo.mod"),
        );
        let frozen_input_fs = super::super::snapshot::ResolverFs::snapshot_global(Arc::new(
            super::super::snapshot::CompileInputSnapshot::default(),
        ));

        assert!(!cached_native_extension_specs_match_frozen_inputs(
            std::slice::from_mut(&mut spec),
            &frozen_input_fs,
            &[],
            &root.join("mod-cache"),
            &vo_module::workspace::WorkspaceDiscovery::Disabled,
        ));

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn stable_build_coordinator_identity_excludes_mutable_cargo_context() {
        let root = temp_dir("vo_native_stable_build_coordinator");
        let module_dir = root.join("module");
        let rust_dir = module_dir.join("rust");
        fs::create_dir_all(&rust_dir).unwrap();
        fs::write(
            module_dir.join("vo.mod"),
            "module = \"github.com/acme/demo\"\n",
        )
        .unwrap();
        let cargo_manifest = rust_dir.join("Cargo.toml");
        fs::write(
            &cargo_manifest,
            "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let module_dir = module_dir.canonicalize().unwrap();
        let cargo_manifest = cargo_manifest.canonicalize().unwrap();
        let manifest = local_manifest(&module_dir);
        let owner = local_extension_module_owner(&manifest).unwrap();
        let identity_before = native_extension_build_coordinator_identity(
            &manifest,
            &module_dir,
            &cargo_manifest,
            &owner,
        )
        .unwrap();

        let base_context = NativeCargoContext {
            manifest_dir: cargo_manifest.parent().unwrap().to_path_buf(),
            manifest_path: cargo_manifest.clone(),
            workspace_root: module_dir.clone(),
            lock_path: module_dir.join("Cargo.lock"),
            target_directory: module_dir.join("target-a"),
            root_package_id: "demo 0.1.0".to_string(),
            root_package_name: "demo".to_string(),
            root_target_name: "demo".to_string(),
            local_package_roots: vec![module_dir.join("rust")],
            patch_configs: Vec::new(),
            metadata_digest: "metadata-a".to_string(),
        };
        let mut drifted_context = base_context.clone();
        drifted_context.target_directory = module_dir.join("target-b");
        drifted_context.metadata_digest = "metadata-b".to_string();
        assert_ne!(
            native_extension_build_file_lock_path(&base_context),
            native_extension_build_file_lock_path(&drifted_context),
        );

        fs::write(
            &cargo_manifest,
            "[package]\nname = \"demo\"\nversion = \"0.1.1\"\n",
        )
        .unwrap();
        let identity_after = native_extension_build_coordinator_identity(
            &manifest,
            &module_dir,
            &cargo_manifest,
            &owner,
        )
        .unwrap();
        assert_eq!(identity_before, identity_after);

        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn native_build_file_lock_serializes_concurrent_builders_for_one_artifact() {
        const WORKERS: usize = 8;
        let root = temp_dir("vo_native_build_lock");
        let profile = if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        };
        let native_path = Arc::new(
            root.join("target")
                .join(profile)
                .join(current_platform_library_name("libdemo")),
        );
        let barrier = Arc::new(Barrier::new(WORKERS));
        let active = Arc::new(AtomicUsize::new(0));
        let max_active = Arc::new(AtomicUsize::new(0));
        let mut workers = Vec::new();

        for _ in 0..WORKERS {
            let native_path = Arc::clone(&native_path);
            let barrier = Arc::clone(&barrier);
            let active = Arc::clone(&active);
            let max_active = Arc::clone(&max_active);
            workers.push(std::thread::spawn(move || {
                barrier.wait();
                let _guard = acquire_native_extension_build_file_lock(&native_path).unwrap();
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
