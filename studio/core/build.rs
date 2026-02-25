//! Build script: embed all studio Vo source files at compile time.
//!
//! Files are stored without prefix so callers control the layout:
//! - Studio app files: `"main.vo"`, `"actions.vo"`, etc.
//! - vogui package:    `"vogui/app.vo"`, `"vogui/widget.vo"`, etc.
//! - vox package:      `"vox/vox.vo"`

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("studio_embedded_files.rs");
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    // studio/core -> studio -> repo root
    let repo_root = manifest_dir.join("../..").canonicalize().unwrap();

    let app_dir = repo_root.join("studio/app");
    let vogui_dir = repo_root.join("libs/vogui");
    let vox_file = repo_root.join("libs/vox/vox.vo");

    let mut entries = String::new();
    embed_vo_dir(&app_dir, "", &mut entries);
    embed_vo_dir(&vogui_dir, "vogui", &mut entries);
    embed_single_vo(&vox_file, "vox/vox.vo", &mut entries);

    let output = format!(
        "pub static STUDIO_EMBEDDED_FILES: &[(&str, &[u8])] = &[\n{entries}];\n"
    );
    fs::write(&dest_path, output).unwrap();

    println!("cargo:rerun-if-changed={}", app_dir.display());
    println!("cargo:rerun-if-changed={}", vogui_dir.display());
    println!("cargo:rerun-if-changed={}", vox_file.display());
}

fn embed_vo_dir(dir: &Path, prefix: &str, out: &mut String) {
    let Ok(entries) = fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("vo") {
            let name = path.file_name().unwrap().to_str().unwrap();
            let vfs_path = if prefix.is_empty() {
                name.to_string()
            } else {
                format!("{}/{}", prefix, name)
            };
            embed_single_vo(&path, &vfs_path, out);
        }
    }
}

fn embed_single_vo(path: &Path, vfs_path: &str, out: &mut String) {
    out.push_str(&format!(
        "    ({:?}, include_bytes!({:?})),\n",
        vfs_path,
        path.display()
    ));
}
