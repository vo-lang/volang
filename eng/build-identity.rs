//! Source identity for a crate and its local production/build dependencies.
//! Optional dependencies are included so identities cover every compiled feature.
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

pub fn emit(manifest_dir: &Path, variable: &str) {
    let root = manifest_dir
        .ancestors()
        .nth(3)
        .expect("crate under lang/crates");
    let workspace = read_manifest(&root.join("Cargo.toml"));
    let mut visited = BTreeSet::new();
    let mut inputs = Vec::new();
    collect_dependency_inputs(manifest_dir, root, &workspace, &mut visited, &mut inputs);
    for path in [
        root.join("Cargo.toml"),
        root.join("Cargo.lock"),
        root.join("eng/build-identity.rs"),
        root.join("lang/stdlib"),
    ] {
        collect_identity_files(&path, &mut inputs);
    }
    inputs.sort();
    inputs.dedup();
    let build_id = compute_build_id(root, &inputs);
    println!("cargo:rustc-env={variable}={build_id}");
    for path in inputs {
        println!("cargo:rerun-if-changed={}", path.display());
    }
}

fn read_manifest(path: &Path) -> toml::Value {
    fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("{}: {error}", path.display()))
        .parse()
        .unwrap_or_else(|error| panic!("{}: {error}", path.display()))
}

fn collect_dependency_inputs(
    dir: &Path,
    root: &Path,
    workspace: &toml::Value,
    visited: &mut BTreeSet<PathBuf>,
    inputs: &mut Vec<PathBuf>,
) {
    let dir = dir.canonicalize().expect("local dependency directory");
    if !visited.insert(dir.clone()) {
        return;
    }
    collect_identity_files(&dir, inputs);
    let manifest = read_manifest(&dir.join("Cargo.toml"));
    let mut sections = vec![&manifest];
    if let Some(targets) = manifest.get("target").and_then(toml::Value::as_table) {
        sections.extend(targets.values());
    }
    for section in sections {
        for kind in ["dependencies", "build-dependencies"] {
            let Some(dependencies) = section.get(kind).and_then(toml::Value::as_table) else {
                continue;
            };
            for (name, declaration) in dependencies {
                let inherited = declaration
                    .get("workspace")
                    .and_then(toml::Value::as_bool)
                    .unwrap_or(false);
                let (base, declaration) = if inherited {
                    (root, &workspace["workspace"]["dependencies"][name])
                } else {
                    (dir.as_path(), declaration)
                };
                if let Some(path) = declaration.get("path").and_then(toml::Value::as_str) {
                    collect_dependency_inputs(&base.join(path), root, workspace, visited, inputs);
                }
            }
        }
    }
}

fn collect_identity_files(path: &Path, out: &mut Vec<PathBuf>) {
    if !path.exists() {
        return;
    }
    if path.is_file() {
        if should_hash_file(path) {
            out.push(path.to_path_buf());
        }
        return;
    }

    let mut entries = fs::read_dir(path)
        .unwrap_or_else(|error| panic!("failed to read {}: {}", path.display(), error))
        .collect::<Result<Vec<_>, _>>()
        .unwrap_or_else(|error| panic!("failed to read {}: {}", path.display(), error));
    entries.sort_by_key(|entry| entry.file_name());
    for entry in entries {
        let child = entry.path();
        if child.is_dir() {
            if should_skip_dir(&child) {
                continue;
            }
            collect_identity_files(&child, out);
            continue;
        }
        if should_hash_file(&child) {
            out.push(child);
        }
    }
}

fn should_skip_dir(path: &Path) -> bool {
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some("target") | Some(".git") | Some("pkg") | Some("pkg-island") | Some("node_modules")
    )
}

fn should_hash_file(path: &Path) -> bool {
    if matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some("Cargo.toml") | Some("Cargo.lock") | Some("build.rs") | Some("stdlib.toml")
    ) {
        return true;
    }
    matches!(
        path.extension().and_then(|ext| ext.to_str()),
        Some("rs") | Some("toml") | Some("vo")
    )
}

fn compute_build_id(workspace_root: &Path, inputs: &[PathBuf]) -> String {
    let mut hash = 0xcbf29ce484222325u64;
    for path in inputs {
        let rel = path.strip_prefix(workspace_root).unwrap_or(path);
        hash = fnv1a_update(hash, rel.to_string_lossy().as_bytes());
        hash = fnv1a_update(hash, &[0]);
        let bytes = fs::read(path)
            .unwrap_or_else(|error| panic!("failed to read {}: {}", path.display(), error));
        hash = fnv1a_update(hash, &bytes);
        hash = fnv1a_update(hash, &[0xff]);
    }
    format!("{hash:016x}")
}

fn fnv1a_update(mut hash: u64, bytes: &[u8]) -> u64 {
    for byte in bytes {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}
