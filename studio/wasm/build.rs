//! Build script: embed shell handler .vo source files for WASM compilation.
//!
//! Only the shell handler's own source files are embedded. Third-party
//! dependencies (vogui, vox, git2, zip) are installed into the JS VFS at
//! runtime via `install_module_to_vfs` and resolved through `WasmVfs`.

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

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    // studio/wasm -> studio -> repo root
    let repo_root = manifest_dir.join("../..").canonicalize().unwrap();

    // ── shell handler source files + vo.mod ─────────────────────────────────
    let mut shell_entries = String::new();
    let shell_dir = repo_root.join("studio/vo/shell");
    collect_vo_files(&shell_dir, "studio/vo/shell", &mut shell_entries, &out_dir);

    // Embed vo.mod so Rust can parse it at runtime to auto-install deps.
    let vo_mod_path = shell_dir.join("vo.mod");
    if vo_mod_path.is_file() {
        shell_entries.push_str(&format!(
            "    ({:?}, include_bytes!({:?})),\n",
            "studio/vo/shell/vo.mod",
            vo_mod_path.display()
        ));
    }

    println!("cargo:rerun-if-changed={}", shell_dir.display());

    fs::write(
        Path::new(&out_dir).join("shell_embedded.rs"),
        format!("pub static SHELL_HANDLER_FILES: &[(&str, &[u8])] = &[\n{}];\n", shell_entries),
    ).unwrap();
}
