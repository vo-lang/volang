use std::path::PathBuf;

#[derive(Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FileDialogFilter {
    pub name: String,
    pub extensions: Vec<String>,
}

#[tauri::command]
pub async fn cmd_pick_directory(default_path: Option<String>) -> Result<Option<String>, String> {
    tauri::async_runtime::spawn_blocking(move || {
        let mut dialog = rfd::FileDialog::new();
        if let Some(ref path) = default_path {
            let p = PathBuf::from(path);
            if p.is_dir() {
                dialog = dialog.set_directory(&p);
            }
        }
        Ok(dialog.pick_folder().map(|p| p.to_string_lossy().to_string()))
    })
    .await
    .map_err(|err| format!("dialog task failed: {err}"))?
}

#[tauri::command]
pub async fn cmd_pick_file(
    default_path: Option<String>,
    filters: Option<Vec<FileDialogFilter>>,
) -> Result<Option<String>, String> {
    tauri::async_runtime::spawn_blocking(move || {
        let mut dialog = rfd::FileDialog::new();
        if let Some(filters) = filters {
            for filter in filters {
                dialog = dialog.add_filter(&filter.name, &filter.extensions);
            }
        } else {
            dialog = dialog.add_filter("Vo source", &["vo"]);
        }
        if let Some(ref path) = default_path {
            let p = PathBuf::from(path);
            if p.is_dir() {
                dialog = dialog.set_directory(&p);
            } else if let Some(parent) = p.parent() {
                if parent.is_dir() {
                    dialog = dialog.set_directory(parent);
                }
            }
        }
        Ok(dialog.pick_file().map(|p| p.to_string_lossy().to_string()))
    })
    .await
    .map_err(|err| format!("dialog task failed: {err}"))?
}
