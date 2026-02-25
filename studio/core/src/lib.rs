//! Shared embedded Vo source files for Vibe Studio.
//!
//! Both the WASM and native (Tauri) studio builds depend on this crate to
//! avoid duplicating the file-embedding logic.
//!
//! # Layout in `STUDIO_EMBEDDED_FILES`
//! - Studio app:  `"main.vo"`, `"actions.vo"`, `"state.vo"`, `"view.vo"` (no path prefix)
//! - vogui pkg:   `"vogui/app.vo"`, `"vogui/widget.vo"`, … (contains `/`)
//! - vox pkg:     `"vox/vox.vo"` (contains `/`)

use std::path::PathBuf;
use vo_common::vfs::MemoryFs;

include!(concat!(env!("OUT_DIR"), "/studio_embedded_files.rs"));

/// Add the studio *package dependencies* (vogui, vox) to a filesystem.
///
/// Pass this filesystem as the stdlib FS so `import "vogui"` and `import "vox"`
/// resolve during compilation.  Package files are those whose embedded path
/// contains a `/` (e.g. `"vogui/app.vo"`).
pub fn add_packages_to_fs(fs: &mut MemoryFs) {
    for (vfs_path, bytes) in STUDIO_EMBEDDED_FILES {
        if vfs_path.contains('/') {
            if let Ok(content) = std::str::from_utf8(bytes) {
                fs.add_file(PathBuf::from(vfs_path), content.to_string());
            }
        }
    }
}

/// Add the studio *app source files* (main.vo, state.vo, …) to a filesystem.
///
/// `pkg_prefix` is the directory to place them under.  Pass `""` for native
/// compilation (files live at the FS root) or `"studio"` for the WASM path
/// (files live under `studio/`).
pub fn add_app_to_fs(fs: &mut MemoryFs, pkg_prefix: &str) {
    for (vfs_path, bytes) in STUDIO_EMBEDDED_FILES {
        if !vfs_path.contains('/') {
            if let Ok(content) = std::str::from_utf8(bytes) {
                let path = if pkg_prefix.is_empty() {
                    PathBuf::from(vfs_path)
                } else {
                    PathBuf::from(format!("{}/{}", pkg_prefix, vfs_path))
                };
                fs.add_file(path, content.to_string());
            }
        }
    }
}

/// Build a `MemoryFs` for native compilation via `vo_engine::compile_from_memory`.
///
/// Studio app files are at the FS root; package directories (`vogui/`, `vox/`)
/// are subdirectories — matching what `compile_from_memory` expects.
pub fn build_native_fs() -> MemoryFs {
    let mut fs = MemoryFs::new();
    add_app_to_fs(&mut fs, "");
    add_packages_to_fs(&mut fs);
    fs
}
