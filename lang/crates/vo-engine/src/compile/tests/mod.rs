use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use vo_module::digest::Digest;
use vo_module::identity::ModulePath;
use vo_module::schema::lockfile::LockedModule;
use vo_module::version::{ExactVersion, ToolchainConstraint};

use super::cache::compile_cache_slot;
use super::native::{
    current_target_triple, prepare_extension_manifests_for_frozen_build, validate_locked_modules_installed,
};
use super::project_prepare::load_project_deps_for_engine;

mod cases;

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
            module: ModulePath::parse(root_module).unwrap(),
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
