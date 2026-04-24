//! Build script: embed term handler .vo source files for WASM compilation.
//!
//! Only the term handler's own source files are embedded. Third-party
//! dependencies (vogui, vox, git2, zip) are installed into the JS VFS at
//! runtime through the project dependency flow and resolved through `WasmVfs`.

use std::{
    env, fs,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

// Stub for http.vo in WASM builds: defines handleHttp without importing net/http.
// http.* ops are not in WASM capabilities so this path is never reached at runtime,
// but the function must exist for the compiler.
const HTTP_STUB_SOURCE: &str = r#"package main

func handleHttp(id, workspace, cwd, kind string, op any) error {
	writeError(id, "ERR_NOT_SUPPORTED", "http ops require native mode")
	return nil
}"#;

const TERM_HANDLER_VFS_ROOT: &str = "studio/vo/term";

// Term handler files to substitute with stubs in WASM (avoid heavy stdlib deps).
const WASM_STUB_TERM_HANDLER_FILES: &[&str] = &["http.vo"];

fn studio_wasm_build_id() -> String {
    if let Ok(value) = env::var("VIBE_STUDIO_BUILD_ID") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            return trimmed.to_string();
        }
    }
    let github_parts = [
        env::var("GITHUB_SHA").ok(),
        env::var("GITHUB_RUN_ID").ok(),
        env::var("GITHUB_RUN_ATTEMPT").ok(),
    ]
    .into_iter()
    .flatten()
    .map(|value| value.trim().to_string())
    .filter(|value| !value.is_empty())
    .collect::<Vec<_>>();
    if !github_parts.is_empty() {
        return github_parts.join("-");
    }
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_millis();
    format!("local-{:x}", now)
}

fn write_studio_wasm_build_info(out_dir: &str) {
    println!("cargo:rerun-if-env-changed=VIBE_STUDIO_BUILD_ID");
    println!("cargo:rerun-if-env-changed=GITHUB_SHA");
    println!("cargo:rerun-if-env-changed=GITHUB_RUN_ID");
    println!("cargo:rerun-if-env-changed=GITHUB_RUN_ATTEMPT");
    let build_id = studio_wasm_build_id();
    fs::write(
        Path::new(out_dir).join("studio_build_info.rs"),
        format!("const STUDIO_WASM_BUILD_ID: &str = {:?};\n", build_id),
    )
    .unwrap();
}

fn collect_vo_files(dir: &Path, prefix: &str, entries: &mut String, out_dir: &str) {
    if let Ok(rd) = fs::read_dir(dir) {
        let mut paths: Vec<_> = rd
            .flatten()
            .map(|e| e.path())
            .filter(|p| p.is_file() && p.extension().and_then(|e| e.to_str()) == Some("vo"))
            .collect();
        paths.sort();
        for path in paths {
            let name = path.file_name().unwrap().to_str().unwrap();
            let vfs_path = format!("{}/{}", prefix, name);
            if WASM_STUB_TERM_HANDLER_FILES.contains(&name) {
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
    write_studio_wasm_build_info(&out_dir);

    // ── term handler source files + manifests ───────────────────────────────
    let mut term_handler_entries = String::new();
    let term_handler_dir = repo_root.join(TERM_HANDLER_VFS_ROOT);
    collect_vo_files(
        &term_handler_dir,
        TERM_HANDLER_VFS_ROOT,
        &mut term_handler_entries,
        &out_dir,
    );

    for manifest_name in ["vo.mod", "vo.lock"] {
        let manifest_path = term_handler_dir.join(manifest_name);
        if manifest_path.is_file() {
            term_handler_entries.push_str(&format!(
                "    ({:?}, include_bytes!({:?})),\n",
                format!("{}/{}", TERM_HANDLER_VFS_ROOT, manifest_name),
                manifest_path.display()
            ));
        }
    }

    println!("cargo:rerun-if-changed={}", term_handler_dir.display());

    fs::write(
        Path::new(&out_dir).join("term_embedded.rs"),
        format!(
            "pub static TERM_HANDLER_FILES: &[(&str, &[u8])] = &[\n{}];\n",
            term_handler_entries
        ),
    )
    .unwrap();
}
