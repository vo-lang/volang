use crate::state::AppState;
use super::pathing::resolve_path;
use vo_engine::prepare_with_auto_install;

#[derive(serde::Serialize, serde::Deserialize, Clone)]
#[serde(rename_all = "camelCase", tag = "kind")]
pub enum InstallEvent {
    Fetch { message: String },
    Build { line: String },
    Done { module: String },
    Error { message: String },
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstalledModule {
    pub spec: String,
    pub path: String,
    pub has_native_ext: bool,
    pub has_wasm_ext: bool,
}

#[tauri::command]
pub async fn cmd_vo_get_stream(
    spec: String,
    on_event: tauri::ipc::Channel<InstallEvent>,
) -> Result<(), String> {
    let spec_clone = spec.clone();
    std::thread::spawn(move || {
        let _ = on_event.send(InstallEvent::Fetch { message: format!("Fetching {}...", spec_clone) });
        match prepare_with_auto_install(&spec_clone) {
            Ok(()) => {
                let _ = on_event.send(InstallEvent::Done { module: spec_clone });
            }
            Err(err) => {
                let _ = on_event.send(InstallEvent::Error { message: err.to_string() });
            }
        }
    });
    Ok(())
}

#[tauri::command]
pub fn cmd_vo_init(path: String, name: Option<String>, state: tauri::State<'_, AppState>) -> Result<String, String> {
    let session_root = state.session_root();
    let dir = resolve_path(&session_root, &path)?;
    if !dir.is_dir() {
        return Err(format!("Not a directory: {}", dir.display()));
    }
    let mod_name = name.unwrap_or_else(|| {
        dir.file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| "main".to_string())
    });
    let mod_file = dir.join("vo.mod");
    if mod_file.exists() {
        return Err(format!("vo.mod already exists in {}", dir.display()));
    }
    let content = format!("module {}\n\nvo 1.0\n", mod_name);
    std::fs::write(&mod_file, &content)
        .map_err(|err| format!("{}: {}", mod_file.display(), err))?;
    let main_vo = dir.join("main.vo");
    if !main_vo.exists() {
        let main_content = format!(
            "package {}\n\nimport \"fmt\"\n\nfunc main() {{\n    fmt.Println(\"Hello, {}!\")\n}}\n",
            mod_name,
            mod_name
        );
        std::fs::write(&main_vo, &main_content)
            .map_err(|err| format!("{}: {}", main_vo.display(), err))?;
    }
    Ok(dir.to_string_lossy().to_string())
}

#[tauri::command]
pub fn cmd_vo_version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[tauri::command]
pub fn cmd_list_installed_modules(state: tauri::State<'_, AppState>) -> Result<Vec<InstalledModule>, String> {
    let _ = state;
    let mod_root = dirs::home_dir()
        .ok_or_else(|| "cannot determine home directory".to_string())?
        .join(".vo")
        .join("mod");
    if !mod_root.exists() {
        return Ok(vec![]);
    }
    let mut modules = Vec::new();
    collect_installed_modules(&mod_root, &mod_root, &mut modules);
    Ok(modules)
}

fn collect_installed_modules(base: &std::path::Path, dir: &std::path::Path, out: &mut Vec<InstalledModule>) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for entry in entries.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.join("vo.mod").is_file() {
            let spec = path.strip_prefix(base)
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|_| path.to_string_lossy().to_string());
            let has_native_ext = path.join("vo.ext.toml").is_file();
            let has_wasm_ext = path.read_dir().ok()
                .map(|entries| entries.filter_map(|e| e.ok())
                    .any(|e| e.path().extension().map(|ext| ext == "wasm").unwrap_or(false)))
                .unwrap_or(false);
            out.push(InstalledModule {
                spec,
                path: path.to_string_lossy().to_string(),
                has_native_ext,
                has_wasm_ext,
            });
        } else if path.is_dir() {
            collect_installed_modules(base, &path, out);
        }
    }
}
