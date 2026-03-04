//! Build script: embed .vo source files for WASM compilation.
//!
//! Embeds:
//! - libs/vogui/ top-level .vo files   → "vogui/<name>"
//! - studio/vo/shell/ .vo files         → "studio/vo/shell/<name>"
//! - 3rdparty/git2/git2.vo              → "3rdparty/git2/git2.vo"
//! - 3rdparty/zip/zip.vo                → "3rdparty/zip/zip.vo"
//! - libs/vox/vox.vo                    → "libs/vox/vox.vo"

use std::{env, fs, path::{Path, PathBuf}};

// Stub for http.vo in WASM builds: defines handleHttp without importing net/http.
// http.* ops are not in WASM capabilities so this path is never reached at runtime,
// but the function must exist for the compiler.
const HTTP_STUB_SOURCE: &str = r#"package main

func handleHttp(id, workspace, cwd, kind string, op any) error {
	writeError(id, "ERR_NOT_SUPPORTED", "http ops require native mode")
	return nil
}
"#;

// Shell handler files to substitute with stubs in WASM (avoid heavy stdlib deps).
const WASM_STUB_SHELL_FILES: &[&str] = &[
    "http.vo",
];

fn collect_vo_files(dir: &Path, prefix: &str, entries: &mut String, out_dir: &str) {
    if let Ok(rd) = fs::read_dir(dir) {
        let mut paths: Vec<_> = rd.flatten()
            .map(|e| e.path())
            .filter(|p| p.is_file() && p.extension().and_then(|e| e.to_str()) == Some("vo"))
            .collect();
        paths.sort();
        for path in paths {
            let name = path.file_name().unwrap().to_str().unwrap();
            let vfs_path = format!("{}/{}", prefix, name);
            if WASM_STUB_SHELL_FILES.contains(&name) {
                // Write stub and embed from OUT_DIR so the function is defined
                // without pulling in heavy stdlib dependencies.
                let stub_name = format!("stub_{}", name);
                let stub_path = Path::new(out_dir).join(&stub_name);
                fs::write(&stub_path, HTTP_STUB_SOURCE).unwrap();
                entries.push_str(&format!(
                    "    ({:?}, include_bytes!({:?})),\n",
                    vfs_path,
                    stub_path.display()
                ));
            } else {
                entries.push_str(&format!(
                    "    ({:?}, include_bytes!({:?})),\n",
                    vfs_path,
                    path.display()
                ));
            }
        }
    }
}

fn embed_single_file(path: &Path, vfs_path: &str, entries: &mut String) {
    if path.is_file() {
        entries.push_str(&format!(
            "    ({:?}, include_bytes!({:?})),\n",
            vfs_path,
            path.display()
        ));
    }
}

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    // studio/wasm -> studio -> repo root
    let repo_root = manifest_dir.join("../..").canonicalize().unwrap();

    // ── vogui ─────────────────────────────────────────────────────────────────
    let vogui_dir = repo_root.join("libs/vogui");
    let mut vogui_entries = String::new();
    collect_vo_files(&vogui_dir, "vogui", &mut vogui_entries, &out_dir);
    fs::write(
        Path::new(&out_dir).join("vogui_embedded.rs"),
        format!("pub static VOGUI_FILES: &[(&str, &[u8])] = &[\n{}];\n", vogui_entries),
    ).unwrap();
    println!("cargo:rerun-if-changed={}", vogui_dir.display());

    // ── shell handler + 3rdparty sources ─────────────────────────────────────
    let mut shell_entries = String::new();

    // studio/vo/shell/*.vo
    let shell_dir = repo_root.join("studio/vo/shell");
    collect_vo_files(&shell_dir, "studio/vo/shell", &mut shell_entries, &out_dir);
    println!("cargo:rerun-if-changed={}", shell_dir.display());

    // 3rdparty/git2/git2.vo
    embed_single_file(
        &repo_root.join("3rdparty/git2/git2.vo"),
        "3rdparty/git2/git2.vo",
        &mut shell_entries,
    );
    println!("cargo:rerun-if-changed={}", repo_root.join("3rdparty/git2/git2.vo").display());

    // 3rdparty/zip/zip.vo
    embed_single_file(
        &repo_root.join("3rdparty/zip/zip.vo"),
        "3rdparty/zip/zip.vo",
        &mut shell_entries,
    );
    println!("cargo:rerun-if-changed={}", repo_root.join("3rdparty/zip/zip.vo").display());

    // libs/vox/vox.vo
    embed_single_file(
        &repo_root.join("libs/vox/vox.vo"),
        "libs/vox/vox.vo",
        &mut shell_entries,
    );
    println!("cargo:rerun-if-changed={}", repo_root.join("libs/vox/vox.vo").display());

    fs::write(
        Path::new(&out_dir).join("shell_embedded.rs"),
        format!("pub static SHELL_HANDLER_FILES: &[(&str, &[u8])] = &[\n{}];\n", shell_entries),
    ).unwrap();
}
