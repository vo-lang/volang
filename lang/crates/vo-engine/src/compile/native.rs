use std::fs;
use std::path::{Path, PathBuf};

use vo_common::vfs::RealFs;
use vo_module::cache::validate::InstalledModuleError;
use vo_module::ext_manifest::ExtensionManifest;
use vo_module::readiness::{check_project_readiness, ReadinessFailure, ReadyModule};
use vo_module::schema::lockfile::LockedModule;
use vo_runtime::ext_loader::NativeExtensionSpec;

#[cfg(test)]
use super::CompileError;
use super::{
    emit_compile_log, CompileLogRecord, ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage,
};

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
    check_project_readiness(&fs, locked_modules, current_target_triple())
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
        ReadinessFailure::SourceNotReady { error } => {
            installed_module_error_to_module_system(*error)
        }
        ReadinessFailure::ExtensionManifestReadFailed {
            module,
            version,
            detail,
            manifest_path,
        } => ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ReadFailed,
            detail,
        )
        .with_module_version(module, version)
        .with_path(&manifest_path),
        ReadinessFailure::ExtensionManifestParseFailed {
            module,
            version,
            detail,
            manifest_path,
        } => ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ParseFailed,
            detail,
        )
        .with_module_version(module, version)
        .with_path(&manifest_path),
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
    let ready_modules = check_frozen_dependency_readiness(locked_modules, mod_root)?;
    prepare_native_extension_specs_with_readiness(manifests, &ready_modules, mod_root)
}

pub(super) fn prepare_native_extension_specs_with_readiness(
    manifests: &[ExtensionManifest],
    ready_modules: &[ReadyModule],
    mod_root: &Path,
) -> Result<Vec<NativeExtensionSpec>, ModuleSystemError> {
    use std::collections::BTreeMap;

    let mod_root = mod_root
        .canonicalize()
        .unwrap_or_else(|_| mod_root.to_path_buf());
    let mut prepared: BTreeMap<PathBuf, NativeExtensionSpec> = BTreeMap::new();
    let native_manifests = manifests
        .iter()
        .filter(|manifest| manifest.native.is_some())
        .collect::<Vec<_>>();

    for manifest in &native_manifests {
        let module_dir = extension_manifest_module_dir(manifest)?;
        if prepared.contains_key(&module_dir) {
            continue;
        }
        let resolved = prepare_extension_spec(manifest, ready_modules, &mod_root)?;
        prepared.insert(module_dir, resolved);
    }

    native_manifests
        .into_iter()
        .map(|manifest| {
            let module_dir = extension_manifest_module_dir(manifest)?;
            prepared.get(&module_dir).cloned().ok_or_else(|| {
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::ValidationFailed,
                    format!(
                        "missing prepared native extension spec for {}",
                        manifest.manifest_path.display(),
                    ),
                )
                .with_path(&manifest.manifest_path)
            })
        })
        .collect()
}

pub(super) fn current_target_triple() -> &'static str {
    env!("VO_TARGET_TRIPLE")
}

fn native_extension_spec(
    manifest: &ExtensionManifest,
    native_path: PathBuf,
) -> NativeExtensionSpec {
    NativeExtensionSpec::new(
        manifest.name.clone(),
        native_path,
        manifest.manifest_path.clone(),
    )
}

fn manifest_local_native_path(
    manifest: &ExtensionManifest,
    module_dir: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    manifest
        .resolve_local_native_path(module_dir)
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
) -> Result<NativeExtensionSpec, ModuleSystemError> {
    let module_dir = extension_manifest_module_dir(manifest)?;
    if !module_dir.starts_with(mod_root) {
        ensure_local_native_extension_built(&module_dir)?;
        return Ok(native_extension_spec(
            manifest,
            manifest_local_native_path(manifest, &module_dir)?,
        ));
    }

    let ready = ready_module_for_cached_extension(&module_dir, mod_root, ready_modules)?;
    prepare_cached_extension_spec(manifest, ready, &module_dir)
}

fn prepare_cached_extension_spec(
    manifest: &ExtensionManifest,
    ready: &ReadyModule,
    module_dir: &Path,
) -> Result<NativeExtensionSpec, ModuleSystemError> {
    let artifact = ready
        .artifacts
        .iter()
        .find(|artifact| artifact.id.kind == "extension-native")
        .ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::Missing,
                format!(
                    "vo.mod does not declare extension-native support for target {} in {}@{}",
                    current_target_triple(),
                    ready.module,
                    ready.version,
                ),
            )
            .with_module_version(ready.module.as_str(), ready.version.to_string())
            .with_path(&manifest.manifest_path)
        })?;
    Ok(native_extension_spec(
        manifest,
        module_dir.join(&artifact.cache_relative_path),
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
            .find(|ready| ready.module == module_path && ready.version == version)
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

pub(super) fn ensure_local_native_extension_built(
    module_dir: &Path,
) -> Result<(), ModuleSystemError> {
    let rust_manifest = module_dir.join("rust").join("Cargo.toml");
    if !rust_manifest.exists() {
        return Ok(());
    }

    let manifests = vo_module::ext_manifest::discover_extensions(module_dir).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ParseFailed,
            format!(
                "failed to discover extensions at {}: {}",
                module_dir.display(),
                e
            ),
        )
        .with_path(module_dir)
    })?;
    let Some(manifest) = manifests.into_iter().next() else {
        return Ok(());
    };
    let Some(native_path) = manifest.resolve_local_native_path(module_dir) else {
        return Ok(());
    };
    if native_path.is_file() {
        if let Ok(lib_mtime) = native_path.metadata().and_then(|m| m.modified()) {
            let rust_dir = module_dir.join("rust").join("src");
            if rust_dir.is_dir() {
                let source_files = walkdir_files(&rust_dir).map_err(|e| {
                    ModuleSystemError::new(
                        ModuleSystemStage::NativeExtension,
                        ModuleSystemErrorKind::ReadFailed,
                        format!(
                            "failed to scan rust sources at {}: {}",
                            rust_dir.display(),
                            e
                        ),
                    )
                    .with_path(&rust_dir)
                })?;
                let all_older = source_files.iter().all(|src| {
                    src.metadata()
                        .and_then(|m| m.modified())
                        .map(|t| t <= lib_mtime)
                        .unwrap_or(false)
                });
                if all_older {
                    emit_compile_log(
                        CompileLogRecord::new("vo-engine", "native_extension_cached")
                            .path(native_path.display().to_string()),
                    );
                    return Ok(());
                }
            }
        }
    }

    build_native_extension(module_dir)
}

fn build_native_extension(module_dir: &Path) -> Result<(), ModuleSystemError> {
    let rust_dir = module_dir.join("rust");
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "native_extension_build_start")
            .path(rust_dir.display().to_string()),
    );
    let output = std::process::Command::new("cargo")
        .arg("build")
        .args(if cfg!(debug_assertions) {
            Vec::<&str>::new()
        } else {
            vec!["--release"]
        })
        .current_dir(&rust_dir)
        .output()
        .map_err(|e| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::BuildFailed,
                format!("failed to run cargo build: {}", e),
            )
            .with_path(&rust_dir)
        })?;
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
        .with_path(&rust_dir));
    }
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "native_extension_build_done")
            .path(rust_dir.display().to_string()),
    );
    Ok(())
}

fn walkdir_files(path: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
    let mut result = Vec::new();
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let p = entry.path();
        if p.is_dir() {
            result.extend(walkdir_files(&p)?);
        } else {
            result.push(p);
        }
    }
    Ok(result)
}
