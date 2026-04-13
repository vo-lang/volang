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
    time::{SystemTime, UNIX_EPOCH},
};

#[path = "src/studio_manifest.rs"]
mod studio_manifest;

use studio_manifest::parse_studio_manifest;
use vo_module::{
    ext_manifest::wasm_extension_from_content,
    schema::modfile::ModFile,
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
const LOCAL_FRAMEWORK_VERSION: &str = "v0.0.0-local";
const LOCAL_FRAMEWORK_TRACKED_INPUTS_FILE: &str = "vo_studio_wasm.local_framework_inputs";
const STUDIO_WASM_BUILD_ID_FILE: &str = "vo_studio_wasm.build_id";

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

fn resolve_local_wasm_asset_path(repo: &Path, asset_name: &str) -> PathBuf {
    let direct = repo.join(asset_name);
    if direct.is_file() {
        return direct;
    }
    if asset_name.contains('/') {
        return direct;
    }
    repo.join("rust").join("pkg-island").join(asset_name)
}

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

fn write_studio_wasm_build_info(repo_root: &Path, out_dir: &str) {
    println!("cargo:rerun-if-env-changed=VIBE_STUDIO_BUILD_ID");
    println!("cargo:rerun-if-env-changed=GITHUB_SHA");
    println!("cargo:rerun-if-env-changed=GITHUB_RUN_ID");
    println!("cargo:rerun-if-env-changed=GITHUB_RUN_ATTEMPT");
    let build_id = studio_wasm_build_id();
    let build_id_path = repo_root
        .join("studio")
        .join("public")
        .join("wasm")
        .join(STUDIO_WASM_BUILD_ID_FILE);
    if let Some(parent) = build_id_path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(&build_id_path, format!("{}\n", build_id)).unwrap();
    fs::write(
        Path::new(out_dir).join("studio_build_info.rs"),
        format!("const STUDIO_WASM_BUILD_ID: &str = {:?};\n", build_id),
    )
    .unwrap();
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

fn discover_repo_roots_from_search_roots(search_roots: &[PathBuf]) -> Vec<PathBuf> {
    let mut repos = Vec::new();
    let mut seen = BTreeSet::new();
    for search_root in search_roots {
        let mut entries: Vec<_> = match fs::read_dir(search_root) {
            Ok(rd) => rd.flatten().map(|entry| entry.path()).filter(|path| path.is_dir()).collect(),
            Err(_) => Vec::new(),
        };
        entries.sort();
        for repo in entries {
            let repo_key = normalize_path(&repo);
            if seen.insert(repo_key) {
                repos.push(repo);
            }
        }
    }
    repos
}

fn discover_local_framework_modules(
    repos: &[PathBuf],
    tracked_base_root: &Path,
) -> Vec<LocalFrameworkModuleSpec> {
    let mut modules = Vec::new();
    let mut seen_module_paths = BTreeSet::new();
    for repo in repos {
        let vo_mod_path = repo.join("vo.mod");
        if !vo_mod_path.is_file() {
            continue;
        }
        let vo_mod_content = fs::read_to_string(&vo_mod_path).unwrap();
        let mod_file = ModFile::parse(&vo_mod_content).unwrap();
        let module_path = mod_file.module.as_str().to_string();
        if !seen_module_paths.insert(module_path.clone()) {
            continue;
        }
        let vo_ext_path = repo.join("vo.ext.toml");
        let vo_ext_content = if vo_ext_path.is_file() {
            Some(fs::read_to_string(&vo_ext_path).unwrap())
        } else {
            None
        };
        let studio_manifest = vo_ext_content
            .as_deref()
            .map(|content| parse_studio_manifest(content, vo_ext_path.to_str().unwrap()).unwrap())
            .flatten();
        let mut files = BTreeSet::new();
        let mut tracked_inputs = BTreeSet::new();
        collect_vo_source_files(repo, repo, &mut files);
        tracked_inputs.insert(tracked_glob_entry(&workspace_relative_path(repo, tracked_base_root), "*.vo"));
        for required in ["vo.mod", "vo.lock", "vo.ext.toml"] {
            let path = repo.join(required);
            if path.is_file() {
                files.insert(required.to_string());
                tracked_inputs.insert(tracked_file_entry(&workspace_relative_path(&path, tracked_base_root)));
            }
        }
        if let Some(studio_manifest) = studio_manifest.as_ref() {
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
                collect_dir_files(parent, repo, &mut files);
                tracked_inputs.insert(tracked_dir_entry(&workspace_relative_path(parent, tracked_base_root)));
            }
            let needs_pkg_island = studio_manifest
                .capabilities
                .iter()
                .any(|capability| capability == "vo_web");
            let pkg_island = repo.join("rust").join("pkg-island");
            if needs_pkg_island && pkg_island.is_dir() {
                collect_dir_files(&pkg_island, repo, &mut files);
                tracked_inputs.insert(tracked_dir_entry(&workspace_relative_path(&pkg_island, tracked_base_root)));
            }
        }
        if let Some(vo_ext_content) = vo_ext_content.as_deref() {
            if let Some(ext) = wasm_extension_from_content(vo_ext_content) {
                let wasm_path = resolve_local_wasm_asset_path(repo, &ext.wasm);
                assert!(wasm_path.is_file(), "missing wasm asset {}", wasm_path.display());
                files.insert(normalize_path(wasm_path.strip_prefix(repo).unwrap()));
                tracked_inputs.insert(tracked_file_entry(&workspace_relative_path(&wasm_path, tracked_base_root)));
                if let Some(js_glue) = ext.js_glue {
                    let js_glue_path = resolve_local_wasm_asset_path(repo, &js_glue);
                    assert!(js_glue_path.is_file(), "missing wasm js glue {}", js_glue_path.display());
                    files.insert(normalize_path(js_glue_path.strip_prefix(repo).unwrap()));
                    tracked_inputs.insert(tracked_file_entry(&workspace_relative_path(&js_glue_path, tracked_base_root)));
                }
            }
        }
        if files.is_empty() {
            continue;
        }
        modules.push(LocalFrameworkModuleSpec {
            module_path,
            repo_path: repo.clone(),
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
    let tracked_base_root = repo_root.parent().unwrap_or(repo_root);
    let mut search_roots = vec![repo_root.to_path_buf()];
    if tracked_base_root != repo_root {
        search_roots.push(tracked_base_root.to_path_buf());
    }
    let repo_roots = discover_repo_roots_from_search_roots(&search_roots);
    let modules = discover_local_framework_modules(&repo_roots, tracked_base_root);
    let mut entries = String::new();
    let mut tracked_inputs = BTreeSet::new();
    let mut watched_paths = BTreeSet::new();
    for search_root in &search_roots {
        watched_paths.insert(search_root.display().to_string());
    }
    for module in modules {
        tracked_inputs.extend(module.tracked_inputs.iter().cloned());
        for tracked_input in &module.tracked_inputs {
            if let Some(path) = tracked_input.strip_prefix("file:") {
                watched_paths.insert(tracked_base_root.join(path).display().to_string());
                continue;
            }
            if let Some(path) = tracked_input.strip_prefix("dir:") {
                watched_paths.insert(tracked_base_root.join(path).display().to_string());
                continue;
            }
            if let Some(rest) = tracked_input.strip_prefix("glob:") {
                let (base, _) = rest.split_once('|').unwrap();
                watched_paths.insert(tracked_base_root.join(base).display().to_string());
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
    write_studio_wasm_build_info(&repo_root, &out_dir);

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
