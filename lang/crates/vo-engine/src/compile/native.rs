use std::fs;
use std::path::{Path, PathBuf};

use vo_module::schema::lockfile::{LockedArtifact, LockedModule};
use vo_module::schema::modfile::ModFile;
use vo_runtime::ext_loader::ExtensionManifest;

use super::{emit_compile_log, CompileError, CompileLogRecord, ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage};

pub(super) fn locked_module_cache_dir(mod_root: &Path, locked: &LockedModule) -> PathBuf {
    vo_module::materialize::cache_dir(mod_root, &locked.path, &locked.version)
}

pub(super) fn locked_module_cache_relative_dir(locked: &LockedModule) -> PathBuf {
    vo_module::materialize::cache_relative_dir(&locked.path, &locked.version)
}

fn read_trimmed_metadata(path: &Path) -> Option<String> {
    let content = fs::read_to_string(path).ok()?;
    let trimmed = content.trim().to_string();
    if trimmed.is_empty() {
        return None;
    }
    Some(trimmed)
}

fn installed_module_version(module_dir: &Path) -> Option<String> {
    read_trimmed_metadata(&module_dir.join(vo_module::materialize::VERSION_MARKER))
}

fn installed_module_source_digest(module_dir: &Path) -> Option<String> {
    read_trimmed_metadata(&module_dir.join(vo_module::materialize::SOURCE_DIGEST_MARKER))
}

pub(super) fn installed_module_release_manifest_digest(
    module_dir: &Path,
) -> Result<Option<String>, ModuleSystemError> {
    let path = module_dir.join("vo.release.json");
    let bytes = match fs::read(&path) {
        Ok(bytes) => bytes,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => {
            return Err(
                ModuleSystemError::new(
                    ModuleSystemStage::CachedModule,
                    ModuleSystemErrorKind::ReadFailed,
                    format!(
                        "failed to read cached release manifest at {}: {}",
                        path.display(),
                        error,
                    ),
                )
                .with_path(&path),
            );
        }
    };
    Ok(Some(vo_module::digest::Digest::from_sha256(&bytes).to_string()))
}

pub(super) fn validate_locked_modules_installed(
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<(), CompileError> {
    for locked in locked_modules {
        validate_locked_module_installed(locked, mod_root)?;
    }
    Ok(())
}

fn validate_locked_module_installed(
    locked: &LockedModule,
    mod_root: &Path,
) -> Result<(), CompileError> {
    let module_dir = locked_module_cache_dir(mod_root, locked);
    validate_installed_module(&module_dir, locked).map_err(CompileError::ModuleSystem)
}

fn validate_installed_module(module_dir: &Path, locked: &LockedModule) -> Result<(), ModuleSystemError> {
    if !module_dir.join("vo.mod").is_file() {
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::Missing,
                format!(
                    "locked module {}@{} is not installed at {}; frozen builds do not auto-install dependencies",
                    locked.path, locked.version, module_dir.display(),
                ),
            )
            .with_locked(locked)
            .with_path(module_dir),
        );
    }

    let installed_content = fs::read_to_string(module_dir.join("vo.mod")).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::CachedModule,
            ModuleSystemErrorKind::ReadFailed,
            format!(
                "invalid cached vo.mod for {}@{}: {}",
                locked.path, locked.version, e
            ),
        )
        .with_locked(locked)
        .with_path(&module_dir.join("vo.mod"))
    })?;
    let installed_mod = ModFile::parse(&installed_content).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::CachedModule,
            ModuleSystemErrorKind::ParseFailed,
            format!(
                "invalid cached vo.mod for {}@{}: {}",
                locked.path, locked.version, e
            ),
        )
        .with_locked(locked)
        .with_path(&module_dir.join("vo.mod"))
    })?;
    if installed_mod.module != locked.path {
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::Mismatch,
                format!(
                    "cached vo.mod module mismatch for {}@{} at {}: expected {}, found {}",
                    locked.path,
                    locked.version,
                    module_dir.display(),
                    locked.path,
                    installed_mod.module,
                ),
            )
            .with_locked(locked)
            .with_path(&module_dir.join("vo.mod")),
        );
    }
    if installed_mod.vo != locked.vo {
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::Mismatch,
                format!(
                    "cached vo.mod toolchain constraint mismatch for {}@{} at {}: expected {}, found {}",
                    locked.path,
                    locked.version,
                    module_dir.display(),
                    locked.vo,
                    installed_mod.vo,
                ),
            )
            .with_locked(locked)
            .with_path(&module_dir.join("vo.mod")),
        );
    }

    let installed_version = installed_module_version(module_dir);
    if installed_version.as_deref() != Some(&locked.version.to_string()) {
        let found = installed_version.unwrap_or_else(|| "<missing .vo-version>".to_string());
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::Mismatch,
                format!(
                    "locked module {}@{} is not installed correctly at {} (found {}); frozen builds do not auto-install dependencies",
                    locked.path, locked.version, module_dir.display(), found,
                ),
            )
            .with_locked(locked)
            .with_path(&module_dir.join(vo_module::materialize::VERSION_MARKER)),
        );
    }

    let source_digest = installed_module_source_digest(module_dir).ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::CachedModule,
            ModuleSystemErrorKind::MissingMetadata,
            format!(
                "locked module {}@{} is missing source digest metadata at {}/.vo-source-digest; frozen builds do not auto-install dependencies",
                locked.path, locked.version, module_dir.display(),
            ),
        )
        .with_locked(locked)
        .with_path(&module_dir.join(vo_module::materialize::SOURCE_DIGEST_MARKER))
    })?;
    if source_digest != locked.source.as_str() {
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::Mismatch,
                format!(
                    "locked module {}@{} source digest mismatch at {}: expected {}, found {}; frozen builds do not auto-install dependencies",
                    locked.path, locked.version, module_dir.display(), locked.source, source_digest,
                ),
            )
            .with_locked(locked)
            .with_path(&module_dir.join(vo_module::materialize::SOURCE_DIGEST_MARKER)),
        );
    }

    let manifest_digest = installed_module_release_manifest_digest(module_dir)?.ok_or_else(|| {
        ModuleSystemError::new(
            ModuleSystemStage::CachedModule,
            ModuleSystemErrorKind::MissingMetadata,
            format!(
                "locked module {}@{} is missing vo.release.json at {}; frozen builds do not auto-install dependencies",
                locked.path, locked.version, module_dir.display(),
            ),
        )
        .with_locked(locked)
        .with_path(&module_dir.join("vo.release.json"))
    })?;
    if manifest_digest != locked.release_manifest.as_str() {
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::CachedModule,
                ModuleSystemErrorKind::Mismatch,
                format!(
                    "locked module {}@{} release manifest digest mismatch at {}: expected {}, found {}; frozen builds do not auto-install dependencies",
                    locked.path, locked.version, module_dir.display(), locked.release_manifest, manifest_digest,
                ),
            )
            .with_locked(locked)
            .with_path(&module_dir.join("vo.release.json")),
        );
    }

    Ok(())
}

pub(super) fn prepare_extension_manifests_for_frozen_build(
    manifests: &[ExtensionManifest],
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<Vec<ExtensionManifest>, ModuleSystemError> {
    use std::collections::BTreeMap;

    let mod_root = mod_root.canonicalize().unwrap_or_else(|_| mod_root.to_path_buf());
    let mut prepared: BTreeMap<PathBuf, ExtensionManifest> = BTreeMap::new();

    for manifest in manifests {
        let module_dir = extension_manifest_module_dir(manifest)?;
        if prepared.contains_key(&module_dir) {
            continue;
        }
        let resolved = prepare_extension_manifest(manifest, locked_modules, &mod_root)?;
        prepared.insert(module_dir, resolved);
    }

    Ok(manifests
        .iter()
        .map(|m| {
            let dir = extension_manifest_module_dir(m).unwrap();
            prepared.get(&dir).cloned().unwrap_or_else(|| m.clone())
        })
        .collect())
}

fn module_identity_from_cache_dir(mod_root: &Path, module_dir: &Path) -> Option<(String, String)> {
    let rel = module_dir.strip_prefix(mod_root).ok()?;
    let components: Vec<&str> = rel
        .components()
        .map(|component| component.as_os_str().to_str().unwrap_or(""))
        .collect();
    if components.len() >= 2 {
        let module_path = components[0].replace('@', "/");
        let version = components[1].to_string();
        if module_path.starts_with("github.com/") && version.starts_with('v') {
            return Some((module_path, version));
        }
    }
    None
}

pub(super) fn current_target_triple() -> &'static str {
    env!("VO_TARGET_TRIPLE")
}


fn extension_manifest_module_dir(manifest: &ExtensionManifest) -> Result<PathBuf, ModuleSystemError> {
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
    Ok(module_dir.canonicalize().unwrap_or_else(|_| module_dir.to_path_buf()))
}

fn prepare_extension_manifest(
    manifest: &ExtensionManifest,
    locked_modules: &[LockedModule],
    mod_root: &Path,
) -> Result<ExtensionManifest, ModuleSystemError> {
    let module_dir = extension_manifest_module_dir(manifest)?;
    if !module_dir.starts_with(mod_root) {
        ensure_local_native_extension_built(&module_dir)?;
        return Ok(manifest.clone());
    }

    let locked = locked_module_for_cached_extension(&module_dir, mod_root, locked_modules)?;
    validate_installed_module(&module_dir, locked)?;
    prepare_cached_extension_manifest(manifest, locked, &module_dir)
}

fn prepare_cached_extension_manifest(
    manifest: &ExtensionManifest,
    locked: &LockedModule,
    module_dir: &Path,
) -> Result<ExtensionManifest, ModuleSystemError> {
    let mut resolved = manifest.clone();
    match find_locked_native_artifact(manifest, locked) {
        Ok(artifact) => {
            validate_locked_native_artifact_bytes(module_dir, locked, artifact)?;
            resolved.native_path = locked_native_artifact_path(module_dir, artifact);
        }
        Err(error) if error.kind == ModuleSystemErrorKind::Missing => {
            ensure_local_native_extension_built(module_dir)?;
        }
        Err(error) => return Err(error),
    }
    Ok(resolved)
}

fn locked_module_for_cached_extension<'a>(
    module_dir: &Path,
    mod_root: &Path,
    locked_modules: &'a [LockedModule],
) -> Result<&'a LockedModule, ModuleSystemError> {
    let (module_path, version) = module_identity_from_cache_dir(mod_root, module_dir).ok_or_else(|| {
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
    locked_modules
        .iter()
        .find(|locked| locked.path.as_str() == module_path && locked.version.to_string() == version)
        .ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "missing locked module metadata for cached extension {}@{}",
                    module_path, version,
                ),
            )
            .with_path(module_dir)
        })
}

fn native_extension_artifact_name(
    manifest: &ExtensionManifest,
    locked: &LockedModule,
) -> Result<String, ModuleSystemError> {
    manifest
        .native_path
        .file_name()
        .and_then(|name| name.to_str())
        .map(str::to_string)
        .ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ValidationFailed,
                format!(
                    "invalid native extension artifact path for {}@{}: {}",
                    locked.path,
                    locked.version,
                    manifest.native_path.display(),
                ),
            )
            .with_locked(locked)
            .with_path(&manifest.native_path)
        })
}

fn find_locked_native_artifact<'a>(
    manifest: &ExtensionManifest,
    locked: &'a LockedModule,
) -> Result<&'a LockedArtifact, ModuleSystemError> {
    let artifact_name = native_extension_artifact_name(manifest, locked)?;
    locked
        .artifacts
        .iter()
        .find(|artifact| {
            artifact.id.kind == "extension-native"
                && artifact.id.target == current_target_triple()
                && artifact.id.name == artifact_name
        })
        .ok_or_else(|| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::Missing,
                format!(
                    "vo.lock does not pin an extension-native artifact for {}@{} ({})",
                    locked.path, locked.version, artifact_name,
                ),
            )
            .with_locked(locked)
            .with_path(&manifest.manifest_path)
        })
}

fn locked_native_artifact_path(module_dir: &Path, artifact: &LockedArtifact) -> PathBuf {
    module_dir.join("artifacts").join(&artifact.id.name)
}

fn validate_locked_native_artifact_bytes(
    module_dir: &Path,
    locked: &LockedModule,
    artifact: &LockedArtifact,
) -> Result<(), ModuleSystemError> {
    let artifact_path = locked_native_artifact_path(module_dir, artifact);
    let bytes = fs::read(&artifact_path).map_err(|error| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ReadFailed,
            format!(
                "failed to read cached native extension artifact for {}@{} at {}: {}",
                locked.path,
                locked.version,
                artifact_path.display(),
                error,
            ),
        )
        .with_locked(locked)
        .with_path(&artifact_path)
    })?;
    if bytes.len() as u64 != artifact.size {
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::VerificationFailed,
                format!(
                    "cached native extension artifact size mismatch for {}@{} ({}): expected {}, found {}",
                    locked.path,
                    locked.version,
                    artifact.id.name,
                    artifact.size,
                    bytes.len(),
                ),
            )
            .with_locked(locked)
            .with_path(&artifact_path),
        );
    }
    let digest = vo_module::digest::Digest::from_sha256(&bytes);
    if digest != artifact.digest {
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::VerificationFailed,
                format!(
                    "cached native extension artifact digest mismatch for {}@{} ({}): expected {}, found {}",
                    locked.path,
                    locked.version,
                    artifact.id.name,
                    artifact.digest,
                    digest,
                ),
            )
            .with_locked(locked)
            .with_path(&artifact_path),
        );
    }
    Ok(())
}

pub(super) fn ensure_local_native_extension_built(module_dir: &Path) -> Result<(), ModuleSystemError> {
    let rust_manifest = module_dir.join("rust").join("Cargo.toml");
    if !rust_manifest.exists() {
        return Ok(());
    }

    let manifests = vo_module::ext_manifest::discover_extensions(module_dir).map_err(|e| {
        ModuleSystemError::new(
            ModuleSystemStage::NativeExtension,
            ModuleSystemErrorKind::ParseFailed,
            format!("failed to discover extensions at {}: {}", module_dir.display(), e),
        )
        .with_path(module_dir)
    })?;
    let Some(manifest) = manifests.into_iter().next() else {
        return Ok(());
    };
    if manifest.native_path.is_file() {
        if let Ok(lib_mtime) = manifest.native_path.metadata().and_then(|m| m.modified()) {
            let rust_dir = module_dir.join("rust").join("src");
            if rust_dir.is_dir() {
                let source_files = walkdir_files(&rust_dir).map_err(|e| {
                    ModuleSystemError::new(
                        ModuleSystemStage::NativeExtension,
                        ModuleSystemErrorKind::ReadFailed,
                        format!("failed to scan rust sources at {}: {}", rust_dir.display(), e),
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
                            .path(manifest.native_path.display().to_string()),
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
        return Err(
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::BuildFailed,
                format!(
                    "cargo build failed for {}: {}",
                    rust_dir.display(),
                    String::from_utf8_lossy(&output.stderr)
                ),
            )
            .with_path(&rust_dir),
        );
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
