use super::pathing::resolve_path;
use crate::state::AppState;
use std::io::Write;
use std::path::{Path, PathBuf};

#[tauri::command]
pub fn cmd_vo_init(
    path: String,
    module: String,
    main_content: String,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    let session_root = state.session_root();
    let requested = Path::new(&path);
    let target = if requested.is_absolute() {
        requested.to_path_buf()
    } else {
        resolve_path(&session_root, &path)?
    };
    let dir = create_module_project(&target, &module, &main_content)?;
    Ok(dir.to_string_lossy().to_string())
}

fn create_module_project(
    target: &Path,
    module: &str,
    main_content: &str,
) -> Result<PathBuf, String> {
    vo_module::ops::render_initial_mod_file(module).map_err(|error| error.to_string())?;
    if main_content.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
        return Err(format!(
            "main.vo exceeds the {}-byte text-file limit",
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
        ));
    }

    let dir = create_module_directory(target)?;
    let result = initialize_module_project(&dir, module, main_content);
    finish_module_creation(dir, result)
}

fn create_module_directory(target: &Path) -> Result<PathBuf, String> {
    let parent = target
        .parent()
        .ok_or_else(|| format!("Module path has no parent: {}", target.display()))?
        .canonicalize()
        .map_err(|error| format!("{}: {error}", target.display()))?;
    let name = target
        .file_name()
        .ok_or_else(|| format!("Module path has no directory name: {}", target.display()))?;
    let dir = parent.join(name);
    std::fs::create_dir(&dir).map_err(|error| format!("{}: {error}", dir.display()))?;
    Ok(dir)
}

fn initialize_module_project(dir: &Path, module: &str, main_content: &str) -> Result<(), String> {
    vo_module::ops::mod_init(dir, module).map_err(|error| error.to_string())?;
    let main_path = dir.join("main.vo");
    let mut main = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&main_path)
        .map_err(|error| format!("{}: {error}", main_path.display()))?;
    main.write_all(main_content.as_bytes())
        .map_err(|error| format!("{}: {error}", main_path.display()))?;
    main.sync_all()
        .map_err(|error| format!("{}: {error}", main_path.display()))
}

fn finish_module_creation(dir: PathBuf, result: Result<(), String>) -> Result<PathBuf, String> {
    match result {
        Ok(()) => Ok(dir),
        Err(error) => match std::fs::remove_dir_all(&dir) {
            Ok(()) => Err(error),
            Err(cleanup_error) => Err(format!(
                "{error}; rollback {} failed: {cleanup_error}",
                dir.display(),
            )),
        },
    }
}

#[tauri::command]
pub fn cmd_vo_version() -> String {
    vo_module::TOOLCHAIN_VERSION.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("vo-studio-{label}-{}-{nonce}", std::process::id()))
    }

    #[test]
    fn create_module_directory_creates_exactly_the_requested_child() {
        let root = temp_dir("module-dir-create");
        std::fs::create_dir_all(&root).unwrap();
        let requested = root.join("demo");

        let prepared = create_module_directory(&requested).unwrap();

        assert_eq!(prepared, root.canonicalize().unwrap().join("demo"));
        assert!(prepared.is_dir());
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn create_module_directory_rejects_an_existing_directory_without_touching_it() {
        let root = temp_dir("module-dir-existing");
        let requested = root.join("demo");
        std::fs::create_dir_all(&requested).unwrap();

        std::fs::write(requested.join("sentinel"), "keep").unwrap();

        let error = create_module_directory(&requested).unwrap_err();

        assert!(error.contains("exists"), "{error}");
        assert_eq!(
            std::fs::read_to_string(requested.join("sentinel")).unwrap(),
            "keep"
        );
        std::fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn create_module_directory_rejects_a_symlink() {
        use std::os::unix::fs::symlink;

        let root = temp_dir("module-dir-symlink");
        let real = root.join("real");
        let alias = root.join("alias");
        std::fs::create_dir_all(&real).unwrap();
        symlink(&real, &alias).unwrap();

        let error = create_module_directory(&alias).unwrap_err();

        assert!(error.contains("exists"), "{error}");
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn studio_version_uses_the_module_protocol_authority() {
        assert_eq!(cmd_vo_version(), vo_module::TOOLCHAIN_VERSION);
    }

    #[test]
    fn create_module_project_writes_the_core_manifest_and_entry_together() {
        let root = temp_dir("module-init");
        std::fs::create_dir_all(&root).unwrap();
        let target = root.join("demo");

        let created = create_module_project(
            &target,
            "github.com/acme/demo",
            "package main\n\nfunc main() {}\n",
        )
        .unwrap();

        assert_eq!(created, root.canonicalize().unwrap().join("demo"));
        let manifest = std::fs::read_to_string(created.join("vo.mod")).unwrap();
        assert_eq!(
            manifest,
            format!(
                "format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"{}\"\n",
                vo_module::TOOLCHAIN_CONSTRAINT,
            )
        );
        assert_eq!(
            std::fs::read_to_string(created.join("main.vo")).unwrap(),
            "package main\n\nfunc main() {}\n"
        );
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn create_module_project_rejects_invalid_input_before_creating_the_target() {
        let root = temp_dir("module-init-invalid");
        std::fs::create_dir_all(&root).unwrap();
        let target = root.join("demo");

        let error = create_module_project(&target, "demo", "package main\n").unwrap_err();
        assert!(error.contains("invalid module path"), "{error}");
        assert!(!target.exists());
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn failed_module_creation_removes_the_complete_new_tree() {
        let root = temp_dir("module-init-rollback");
        std::fs::create_dir_all(&root).unwrap();
        let target = create_module_directory(&root.join("demo")).unwrap();
        std::fs::write(target.join("partial"), "partial").unwrap();

        let error = finish_module_creation(target.clone(), Err("injected failure".to_string()))
            .unwrap_err();

        assert_eq!(error, "injected failure");
        assert!(!target.exists());
        std::fs::remove_dir_all(root).unwrap();
    }
}
