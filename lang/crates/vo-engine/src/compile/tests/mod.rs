use std::collections::HashMap;
use std::fs;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::time::SystemTime;

use vo_common::vfs::FileSystem;
use vo_module::digest::Digest;
use vo_module::identity::{ModIdentity, ModulePath};
use vo_module::project::ProjectDeps;
use vo_module::registry::Registry;
use vo_module::schema::lockfile::LockedModule;
use vo_module::schema::manifest::{ManifestRequire, ManifestSource, ReleaseManifest};
use vo_module::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use vo_module::Error;

use super::cache::compile_cache_slot;
use super::native::{
    current_target_triple, prepare_native_extension_specs_for_frozen_build,
    validate_locked_modules_installed,
};
use super::CompileError;

mod cases;

fn load_project_deps_for_engine<F: FileSystem>(
    fs: &F,
    workspace_replaces: &std::collections::HashMap<String, PathBuf>,
) -> Result<ProjectDeps, CompileError> {
    let excluded_modules = workspace_replaces.keys().cloned().collect::<Vec<_>>();
    let project_deps = vo_module::project::read_project_deps(fs, &excluded_modules)
        .map_err(super::module_system_error_from_project)?;
    Ok(project_deps)
}

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

fn locked_module_cache_dir(mod_root: &Path, locked: &LockedModule) -> PathBuf {
    vo_module::cache::layout::cache_dir(mod_root, &locked.path, &locked.version)
}

fn installed_module_release_manifest_digest(module_dir: &Path) -> std::io::Result<Option<String>> {
    match fs::read(module_dir.join("vo.release.json")) {
        Ok(bytes) => Ok(Some(Digest::from_sha256(&bytes).to_string())),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(error),
    }
}

fn make_locked(
    path: &str,
    version: &str,
    vo: &str,
    commit: &str,
    release_manifest: &str,
    source: &str,
) -> LockedModule {
    LockedModule {
        path: ModulePath::parse(path).unwrap(),
        version: ExactVersion::parse(version).unwrap(),
        vo: ToolchainConstraint::parse(vo).unwrap(),
        commit: commit.to_string(),
        release_manifest: Digest::parse(release_manifest).unwrap(),
        source: Digest::parse(source).unwrap(),
        deps: Vec::new(),
        artifacts: Vec::new(),
    }
}

fn render_lock_with_modules(root_module: &str, root_vo: &str, modules: &[LockedModule]) -> String {
    use vo_module::schema::lockfile::{LockFile, LockRoot};
    let lf = LockFile {
        version: 1,
        created_by: "vo test".to_string(),
        root: LockRoot {
            module: ModIdentity::parse(root_module).unwrap(),
            vo: ToolchainConstraint::parse(root_vo).unwrap(),
        },
        resolved: modules.to_vec(),
    };
    lf.render()
}

fn read_saved_cache_fingerprint(root: &Path, single_file: Option<&std::ffi::OsStr>) -> String {
    let slot = compile_cache_slot(root, single_file);
    fs::read_to_string(slot.fingerprint_file)
        .unwrap()
        .trim()
        .to_string()
}

fn write_minimal_native_extension_crate(rust_dir: &Path, crate_name: &str) {
    let vo_ext_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("vo-ext");
    let vo_ext_path = vo_ext_path.to_string_lossy().replace('\\', "/");
    fs::create_dir_all(rust_dir.join("src")).unwrap();
    fs::write(
        rust_dir.join("Cargo.toml"),
        format!(
            "[package]\nname = \"{}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[lib]\ncrate-type = [\"cdylib\"]\n\n[dependencies]\nvo-ext = {{ path = \"{}\" }}\n",
            crate_name, vo_ext_path,
        ),
    )
    .unwrap();
    fs::write(
        rust_dir.join("src").join("lib.rs"),
        "vo_ext::export_extensions!();\n",
    )
    .unwrap();
}

struct MockRegistry {
    versions: HashMap<String, Vec<ExactVersion>>,
    manifests: HashMap<(String, String), ReleaseManifest>,
    sources: HashMap<(String, String, String), Vec<u8>>,
    source_fetches: AtomicUsize,
}

impl MockRegistry {
    fn new() -> Self {
        Self {
            versions: HashMap::new(),
            manifests: HashMap::new(),
            sources: HashMap::new(),
            source_fetches: AtomicUsize::new(0),
        }
    }

    fn add_module(
        &mut self,
        module: &str,
        version: &str,
        vo: &str,
        deps: &[(&str, &str)],
        files: &[(&str, &str)],
    ) {
        let module_path = ModulePath::parse(module).unwrap();
        let exact_version = ExactVersion::parse(version).unwrap();
        let mut require = deps
            .iter()
            .map(|(path, constraint)| ManifestRequire {
                module: ModulePath::parse(path).unwrap(),
                constraint: DepConstraint::parse(constraint).unwrap(),
            })
            .collect::<Vec<_>>();
        require.sort_by(|left, right| left.module.cmp(&right.module));

        let archive_root = format!("{}-{}", module.replace('/', "-"), exact_version.to_string());
        let source_name = format!("{}-source.tar.gz", exact_version);
        let source_bytes = build_source_archive(&archive_root, files);
        let manifest = ReleaseManifest {
            schema_version: 1,
            module: module_path.clone(),
            version: exact_version.clone(),
            commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
            module_root: module_path.module_root().to_string(),
            vo: ToolchainConstraint::parse(vo).unwrap(),
            require,
            source: ManifestSource {
                name: source_name.clone(),
                size: source_bytes.len() as u64,
                digest: Digest::from_sha256(&source_bytes),
            },
            artifacts: Vec::new(),
        };

        self.versions
            .entry(module.to_string())
            .or_default()
            .push(exact_version.clone());
        self.versions.get_mut(module).unwrap().sort();
        self.manifests
            .insert((module.to_string(), exact_version.to_string()), manifest);
        self.sources.insert(
            (module.to_string(), exact_version.to_string(), source_name),
            source_bytes,
        );
    }

    fn source_fetches(&self) -> usize {
        self.source_fetches.load(AtomicOrdering::Relaxed)
    }
}

impl Registry for MockRegistry {
    fn list_versions(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        Ok(self
            .versions
            .get(module.as_str())
            .cloned()
            .unwrap_or_default())
    }

    fn fetch_manifest(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<ReleaseManifest, Error> {
        self.manifests
            .get(&(module.as_str().to_string(), version.to_string()))
            .cloned()
            .ok_or_else(|| Error::RegistryError(format!("no manifest for {} {}", module, version)))
    }

    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        self.source_fetches.fetch_add(1, AtomicOrdering::Relaxed);
        self.sources
            .get(&(
                module.as_str().to_string(),
                version.to_string(),
                asset_name.to_string(),
            ))
            .cloned()
            .ok_or_else(|| {
                Error::RegistryError(format!(
                    "no source package for {} {} {}",
                    module, version, asset_name
                ))
            })
    }

    fn fetch_artifact(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        Err(Error::RegistryError(
            "artifacts not implemented in mock".to_string(),
        ))
    }
}

fn build_source_archive(root: &str, files: &[(&str, &str)]) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    for (relative_path, content) in files {
        let full_path = format!("{root}/{relative_path}");
        let bytes = content.as_bytes();
        let mut header = tar::Header::new_gnu();
        header.set_size(bytes.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        builder
            .append_data(&mut header, full_path, Cursor::new(bytes))
            .unwrap();
    }
    builder.into_inner().unwrap().finish().unwrap()
}
