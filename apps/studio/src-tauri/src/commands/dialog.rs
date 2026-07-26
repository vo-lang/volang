use std::path::PathBuf;

use crate::state::AppState;

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
        Ok(dialog
            .pick_folder()
            .map(|p| p.to_string_lossy().to_string()))
    })
    .await
    .map_err(|err| format!("dialog task failed: {err}"))?
}

#[tauri::command]
pub async fn cmd_pick_file(
    default_path: Option<String>,
    filters: Option<Vec<FileDialogFilter>>,
    state: tauri::State<'_, AppState>,
) -> Result<Option<String>, String> {
    let selected = tauri::async_runtime::spawn_blocking(move || {
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
        Ok::<Option<String>, String>(dialog.pick_file().map(|p| p.to_string_lossy().to_string()))
    })
    .await
    .map_err(|err| format!("dialog task failed: {err}"))??;
    if let Some(path) = selected.as_ref() {
        state.grant_gui_file(PathBuf::from(path), false);
    }
    Ok(selected)
}

#[tauri::command]
pub async fn cmd_save_file(
    default_path: Option<String>,
    filters: Option<Vec<FileDialogFilter>>,
    state: tauri::State<'_, AppState>,
) -> Result<Option<String>, String> {
    let selected = tauri::async_runtime::spawn_blocking(move || {
        let mut dialog = rfd::FileDialog::new();
        if let Some(filters) = filters {
            for filter in filters {
                dialog = dialog.add_filter(&filter.name, &filter.extensions);
            }
        }
        if let Some(ref path) = default_path {
            let p = PathBuf::from(path);
            if p.is_dir() {
                dialog = dialog.set_directory(&p);
            } else {
                if let Some(parent) = p.parent().filter(|parent| parent.is_dir()) {
                    dialog = dialog.set_directory(parent);
                }
                if let Some(name) = p.file_name() {
                    dialog = dialog.set_file_name(name.to_string_lossy());
                }
            }
        }
        Ok::<Option<String>, String>(dialog.save_file().map(|p| p.to_string_lossy().to_string()))
    })
    .await
    .map_err(|err| format!("dialog task failed: {err}"))??;
    if let Some(path) = selected.as_ref() {
        state.grant_gui_file(PathBuf::from(path), true);
    }
    Ok(selected)
}
