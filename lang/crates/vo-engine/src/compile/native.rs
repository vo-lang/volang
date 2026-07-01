use std::fs;
use std::path::{Path, PathBuf};

use vo_common::{stable_hash::StableHasher, vfs::RealFs};
use vo_module::cache::validate::InstalledModuleError;
use vo_module::ext_manifest::ExtensionManifest;
use vo_module::readiness::{check_project_readiness, ReadinessFailure, ReadyModule};
use vo_module::schema::lockfile::LockedModule;
use vo_runtime::ext_loader::{ExtensionLoader, NativeExtensionSpec, ABI_FINGERPRINT, ABI_VERSION};

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
        let resolved = prepare_extension_spec(manifest, ready_modules, &mod_root)?;
        specs.push(resolved);
    }

    Ok(specs)
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
        let native_path = manifest_local_native_path(manifest, &module_dir)?;
        let load_path = prepare_local_native_extension_load_path(&module_dir, &native_path)?;
        return Ok(native_extension_spec(manifest, load_path));
    }

    let ready = ready_module_for_cached_extension(&module_dir, mod_root, ready_modules)?;
    prepare_cached_extension_spec(manifest, ready, &module_dir)
}

fn prepare_local_native_extension_load_path(
    module_dir: &Path,
    native_path: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    let rust_manifest = module_dir.join("rust").join("Cargo.toml");
    if !rust_manifest.exists() {
        return Ok(native_path.to_path_buf());
    }
    ensure_local_native_extension_built(module_dir, native_path)
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
    native_path: &Path,
) -> Result<PathBuf, ModuleSystemError> {
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
    if native_extension_cache_is_valid(native_path, module_dir)? {
        emit_compile_log(
            CompileLogRecord::new("vo-engine", "native_extension_cached")
                .path(native_path.display().to_string()),
        );
        return materialize_native_extension_load_copy(native_path);
    }
    build_native_extension(module_dir)?;
    let load_path = materialize_native_extension_load_copy(native_path)?;
    validate_built_native_extension_abi(&load_path)?;
    write_native_extension_abi_marker(native_path)?;
    write_native_extension_abi_marker(&load_path)?;
    Ok(load_path)
}

fn native_extension_cache_is_valid(
    native_path: &Path,
    module_dir: &Path,
) -> Result<bool, ModuleSystemError> {
    if !native_path.is_file() || !native_extension_build_marker_matches(native_path) {
        return Ok(false);
    }
    let lib_mtime = native_path
        .metadata()
        .and_then(|m| m.modified())
        .map_err(|e| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ReadFailed,
                format!(
                    "failed to read native extension metadata at {}: {}",
                    native_path.display(),
                    e
                ),
            )
            .with_path(native_path)
        })?;

    let mut inputs = vec![module_dir.join("rust").join("Cargo.toml")];
    let lock_path = module_dir.join("rust").join("Cargo.lock");
    if lock_path.is_file() {
        inputs.push(lock_path);
    }
    let build_rs = module_dir.join("rust").join("build.rs");
    if build_rs.is_file() {
        inputs.push(build_rs);
    }
    let rust_src = module_dir.join("rust").join("src");
    if rust_src.is_dir() {
        inputs.extend(walkdir_files(&rust_src).map_err(|e| {
            ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::ReadFailed,
                format!(
                    "failed to scan rust sources at {}: {}",
                    rust_src.display(),
                    e
                ),
            )
            .with_path(&rust_src)
        })?);
    }

    for input in inputs {
        let Ok(input_mtime) = input.metadata().and_then(|m| m.modified()) else {
            return Ok(false);
        };
        if input_mtime > lib_mtime {
            return Ok(false);
        }
    }
    Ok(true)
}

fn materialize_native_extension_load_copy(
    native_path: &Path,
) -> Result<PathBuf, ModuleSystemError> {
    let bytes = fs::read(native_path).map_err(|e| {
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
    let mut hasher = StableHasher::new("vo-native-extension-load-copy-v1");
    hasher.update_str("abi", &current_native_extension_abi_marker());
    hasher.update_bytes("dylib", &bytes);
    let digest = hasher.finish_suffix();
    let copy_id = &digest[..16];
    let load_path = native_extension_load_copy_path(native_path, copy_id);
    if !load_path.is_file() {
        fs::write(&load_path, &bytes).map_err(|e| {
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
    let Ok(marker) = fs::read_to_string(&marker_path) else {
        return false;
    };
    if marker != current_native_extension_abi_marker() {
        return false;
    }
    let Ok(native_mtime) = native_path.metadata().and_then(|m| m.modified()) else {
        return false;
    };
    let Ok(marker_mtime) = marker_path.metadata().and_then(|m| m.modified()) else {
        return false;
    };
    marker_mtime >= native_mtime
}

fn native_extension_host_abi_error(native_path: &Path) -> Option<String> {
    let mut loader = ExtensionLoader::new();
    loader
        .load(native_path, "__abi_check")
        .err()
        .map(|e| e.to_string())
}

fn validate_built_native_extension_abi(native_path: &Path) -> Result<(), ModuleSystemError> {
    if let Some(detail) = native_extension_host_abi_error(native_path) {
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
    fs::write(&marker_path, current_native_extension_abi_marker()).map_err(|e| {
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

fn current_native_extension_abi_marker() -> String {
    format!("version={ABI_VERSION}\nfingerprint={ABI_FINGERPRINT:#x}\n")
}

fn build_native_extension(module_dir: &Path) -> Result<(), ModuleSystemError> {
    let rust_dir = module_dir.join("rust");
    emit_compile_log(
        CompileLogRecord::new("vo-engine", "native_extension_build_start")
            .path(rust_dir.display().to_string()),
    );
    let mut command = std::process::Command::new("cargo");
    command.arg("build");
    for config in local_volang_cargo_patch_configs() {
        command.arg("--config").arg(config);
    }
    if !cfg!(debug_assertions) {
        command.arg("--release");
    }
    let lock_snapshot = CargoLockSnapshot::capture(&rust_dir)?;
    let output_result = command.current_dir(&rust_dir).output();
    let restore_result = lock_snapshot.restore();
    let output = match output_result {
        Ok(output) => output,
        Err(e) => {
            restore_result?;
            return Err(ModuleSystemError::new(
                ModuleSystemStage::NativeExtension,
                ModuleSystemErrorKind::BuildFailed,
                format!("failed to run cargo build: {}", e),
            )
            .with_path(&rust_dir));
        }
    };
    restore_result?;
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

struct CargoLockSnapshot {
    path: PathBuf,
    bytes: Option<Vec<u8>>,
}

impl CargoLockSnapshot {
    fn capture(rust_dir: &Path) -> Result<Self, ModuleSystemError> {
        let path = rust_dir.join("Cargo.lock");
        let bytes = match fs::read(&path) {
            Ok(bytes) => Some(bytes),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
            Err(e) => {
                return Err(ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::ReadFailed,
                    format!("failed to read Cargo lockfile at {}: {}", path.display(), e),
                )
                .with_path(&path));
            }
        };
        Ok(Self { path, bytes })
    }

    fn restore(self) -> Result<(), ModuleSystemError> {
        match self.bytes {
            Some(bytes) => fs::write(&self.path, bytes).map_err(|e| {
                ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::BuildFailed,
                    format!(
                        "failed to restore Cargo lockfile at {}: {}",
                        self.path.display(),
                        e
                    ),
                )
                .with_path(&self.path)
            }),
            None => match fs::remove_file(&self.path) {
                Ok(()) => Ok(()),
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
                Err(e) => Err(ModuleSystemError::new(
                    ModuleSystemStage::NativeExtension,
                    ModuleSystemErrorKind::BuildFailed,
                    format!(
                        "failed to remove generated Cargo lockfile at {}: {}",
                        self.path.display(),
                        e
                    ),
                )
                .with_path(&self.path)),
            },
        }
    }
}

fn local_volang_cargo_patch_configs() -> Vec<String> {
    let crates_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .map(Path::to_path_buf);
    let Some(crates_root) = crates_root else {
        return Vec::new();
    };
    let packages = [
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
        ("vo-syntax", crates_root.join("vo-syntax")),
        ("vo-vm", crates_root.join("vo-vm")),
        ("vo-web", crates_root.join("vo-web")),
    ];
    if packages
        .iter()
        .any(|(_, path)| !path.join("Cargo.toml").is_file())
    {
        return Vec::new();
    }

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

fn toml_quote(value: &str) -> String {
    let escaped = value.replace('\\', "\\\\").replace('"', "\\\"");
    format!("\"{}\"", escaped)
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

#[cfg(test)]
mod tests {
    use super::*;
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

    #[test]
    fn native_extension_specs_dedupe_duplicate_module_manifests() {
        let root = temp_dir("vo_native_ext_dedupe");
        let mod_root = root.join("mod-cache");
        let module_dir = root.join("demo");
        fs::create_dir_all(&module_dir).unwrap();
        fs::create_dir_all(&mod_root).unwrap();

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

        fs::remove_dir_all(&root).unwrap();
    }
}
