//! Build script: embed term handler .vo source files for WASM compilation.
//!
//! Only the term handler's own source files are embedded. Third-party
//! dependencies (vogui, vox, git2, zip) are installed into the JS VFS at
//! runtime via `install_module_to_vfs` and resolved through `WasmVfs`.

use std::{
    collections::BTreeSet,
    env,
    fs,
    path::{Path, PathBuf},
};

#[path = "src/studio_manifest.rs"]
mod studio_manifest;

use studio_manifest::parse_studio_manifest;
use vo_module::{ext_manifest::wasm_extension_from_content, schema::modfile::ModFile};

// Stub for http.vo in WASM builds: defines handleHttp without importing net/http.
// http.* ops are not in WASM capabilities so this path is never reached at runtime,
// but the function must exist for the compiler.
const HTTP_STUB_SOURCE: &str = r#"package main

func handleHttp(id, workspace, cwd, kind string, op any) error {
	writeError(id, "ERR_NOT_SUPPORTED", "http ops require native mode")
	return nil
}
"#;

const TERM_HANDLER_VFS_ROOT: &str = "studio/vo/term";
const LOCAL_FRAMEWORK_VERSION: &str = "v0.0.0-local";
const LOCAL_FRAMEWORK_TRACKED_INPUTS_FILE: &str = "vo_studio_wasm.local_framework_inputs";

// Term handler files to substitute with stubs in WASM (avoid heavy stdlib deps).
const WASM_STUB_TERM_HANDLER_FILES: &[&str] = &[
    "http.vo",
];

struct LocalFrameworkModuleSpec {
    module_path: String,
    repo_path: PathBuf,
    files: BTreeSet<String>,
    tracked_inputs: BTreeSet<String>,
}

fn normalize_path(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

fn workspace_relative_path(path: &Path, workspace_root: &Path) -> String {
    normalize_path(path.strip_prefix(workspace_root).unwrap())
}

fn tracked_file_entry(path: &str) -> String {
    format!("file:{}", path)
}

fn tracked_dir_entry(path: &str) -> String {
    format!("dir:{}", path)
}

fn tracked_glob_entry(base: &str, pattern: &str) -> String {
    format!("glob:{}|{}", base, pattern)
}

fn should_skip_embedded_dir(name: &str) -> bool {
    matches!(name, ".git" | "node_modules" | "target" | ".vo-cache")
}

fn collect_vo_source_files(
    dir: &Path,
    repo_root: &Path,
    files: &mut BTreeSet<String>,
) {
    let Ok(rd) = fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<_> = rd.flatten().map(|entry| entry.path()).collect();
    paths.sort();
    for path in paths {
        if path.is_dir() {
            let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };
            if should_skip_embedded_dir(name) {
                continue;
            }
            collect_vo_source_files(&path, repo_root, files);
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) != Some("vo") {
            continue;
        }
        if let Ok(rel) = path.strip_prefix(repo_root) {
            files.insert(normalize_path(rel));
        }
    }
}

fn collect_dir_files(
    dir: &Path,
    repo_root: &Path,
    files: &mut BTreeSet<String>,
) {
    let Ok(rd) = fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<_> = rd.flatten().map(|entry| entry.path()).collect();
    paths.sort();
    for path in paths {
        if path.is_dir() {
            let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };
            if should_skip_embedded_dir(name) {
                continue;
            }
            collect_dir_files(&path, repo_root, files);
            continue;
        }
        if let Ok(rel) = path.strip_prefix(repo_root) {
            files.insert(normalize_path(rel));
        }
    }
}

fn discover_local_framework_modules(workspace_root: &Path) -> Vec<LocalFrameworkModuleSpec> {
    let mut repos: Vec<_> = match fs::read_dir(workspace_root) {
        Ok(rd) => rd.flatten().map(|entry| entry.path()).filter(|path| path.is_dir()).collect(),
        Err(_) => Vec::new(),
    };
    repos.sort();
    let mut modules = Vec::new();
    for repo in repos {
        let vo_mod_path = repo.join("vo.mod");
        let vo_ext_path = repo.join("vo.ext.toml");
        if !vo_mod_path.is_file() || !vo_ext_path.is_file() {
            continue;
        }
        let vo_mod_content = fs::read_to_string(&vo_mod_path).unwrap();
        let mod_file = ModFile::parse(&vo_mod_content).unwrap();
        let vo_ext_content = fs::read_to_string(&vo_ext_path).unwrap();
        let Some(studio_manifest) = parse_studio_manifest(&vo_ext_content, vo_ext_path.to_str().unwrap()).unwrap() else {
            continue;
        };
        let mut files = BTreeSet::new();
        let mut tracked_inputs = BTreeSet::new();
        collect_vo_source_files(&repo, &repo, &mut files);
        tracked_inputs.insert(tracked_glob_entry(&workspace_relative_path(&repo, workspace_root), "*.vo"));
        for required in ["vo.mod", "vo.lock", "vo.ext.toml"] {
            let path = repo.join(required);
            if path.is_file() {
                files.insert(required.to_string());
                tracked_inputs.insert(tracked_file_entry(&workspace_relative_path(&path, workspace_root)));
            }
        }
        for asset in [
            studio_manifest.renderer_path.as_deref(),
            studio_manifest.protocol_path.as_deref(),
            studio_manifest.host_bridge_path.as_deref(),
        ]
        .into_iter()
        .flatten()
        {
            let asset_path = repo.join(asset);
            assert!(asset_path.is_file(), "missing studio asset {}", asset_path.display());
            let parent = asset_path.parent().unwrap();
            collect_dir_files(parent, &repo, &mut files);
            tracked_inputs.insert(tracked_dir_entry(&workspace_relative_path(parent, workspace_root)));
        }
        if let Some(ext) = wasm_extension_from_content(&vo_ext_content) {
            let wasm_path = repo.join(&ext.wasm);
            assert!(wasm_path.is_file(), "missing wasm asset {}", wasm_path.display());
            files.insert(normalize_path(wasm_path.strip_prefix(&repo).unwrap()));
            tracked_inputs.insert(tracked_file_entry(&workspace_relative_path(&wasm_path, workspace_root)));
            if let Some(js_glue) = ext.js_glue {
                let js_glue_path = repo.join(&js_glue);
                assert!(js_glue_path.is_file(), "missing wasm js glue {}", js_glue_path.display());
                files.insert(normalize_path(js_glue_path.strip_prefix(&repo).unwrap()));
                tracked_inputs.insert(tracked_file_entry(&workspace_relative_path(&js_glue_path, workspace_root)));
            }
        }
        if files.is_empty() {
            continue;
        }
        modules.push(LocalFrameworkModuleSpec {
            module_path: mod_file.module.as_str().to_string(),
            repo_path: repo,
            files,
            tracked_inputs,
        });
    }
    modules
}

fn write_local_framework_modules(repo_root: &Path, out_dir: &str) {
    let out_path = Path::new(out_dir).join("local_framework_modules_embedded.rs");
    let tracked_inputs_path = repo_root
        .join("studio")
        .join("public")
        .join("wasm")
        .join(LOCAL_FRAMEWORK_TRACKED_INPUTS_FILE);
    let Some(workspace_root) = repo_root.parent() else {
        fs::write(&out_path, "static LOCAL_FRAMEWORK_MODULES: &[EmbeddedLocalFrameworkModule] = &[];\n").unwrap();
        let tracked_parent = tracked_inputs_path.parent().unwrap();
        fs::create_dir_all(tracked_parent).unwrap();
        fs::write(&tracked_inputs_path, "").unwrap();
        return;
    };
    let modules = discover_local_framework_modules(workspace_root);
    let mut entries = String::new();
    let mut tracked_inputs = BTreeSet::new();
    let mut watched_paths = BTreeSet::new();
    watched_paths.insert(workspace_root.display().to_string());
    for module in modules {
        tracked_inputs.extend(module.tracked_inputs.iter().cloned());
        for tracked_input in &module.tracked_inputs {
            if let Some(path) = tracked_input.strip_prefix("file:") {
                watched_paths.insert(workspace_root.join(path).display().to_string());
                continue;
            }
            if let Some(path) = tracked_input.strip_prefix("dir:") {
                watched_paths.insert(workspace_root.join(path).display().to_string());
                continue;
            }
            if let Some(rest) = tracked_input.strip_prefix("glob:") {
                let (base, _) = rest.split_once('|').unwrap();
                watched_paths.insert(workspace_root.join(base).display().to_string());
            }
        }
        entries.push_str("    EmbeddedLocalFrameworkModule {\n");
        entries.push_str(&format!("        module_path: {:?},\n", module.module_path));
        entries.push_str(&format!("        version: {:?},\n", LOCAL_FRAMEWORK_VERSION));
        entries.push_str("        files: &[\n");
        for rel in &module.files {
            watched_paths.insert(module.repo_path.join(rel).display().to_string());
            entries.push_str(&format!(
                "            EmbeddedFile {{ path: {:?}, bytes: include_bytes!({:?}) }},\n",
                rel,
                module.repo_path.join(rel).display().to_string()
            ));
        }
        entries.push_str("        ],\n");
        entries.push_str("    },\n");
    }
    for watched_path in watched_paths {
        println!("cargo:rerun-if-changed={}", watched_path);
    }
    let tracked_parent = tracked_inputs_path.parent().unwrap();
    fs::create_dir_all(tracked_parent).unwrap();
    fs::write(
        &tracked_inputs_path,
        if tracked_inputs.is_empty() {
            String::new()
        } else {
            format!("{}\n", tracked_inputs.into_iter().collect::<Vec<_>>().join("\n"))
        },
    )
    .unwrap();
    fs::write(
        &out_path,
        format!(
            "static LOCAL_FRAMEWORK_MODULES: &[EmbeddedLocalFrameworkModule] = &[\n{}];\n",
            entries
        ),
    )
    .unwrap();
}

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

    // ── term handler source files + manifests ───────────────────────────────
    let mut term_handler_entries = String::new();
    let term_handler_dir = repo_root.join(TERM_HANDLER_VFS_ROOT);
    collect_vo_files(&term_handler_dir, TERM_HANDLER_VFS_ROOT, &mut term_handler_entries, &out_dir);

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
        format!("pub static TERM_HANDLER_FILES: &[(&str, &[u8])] = &[\n{}];\n", term_handler_entries),
    ).unwrap();
    write_local_framework_modules(&repo_root, &out_dir);
}
