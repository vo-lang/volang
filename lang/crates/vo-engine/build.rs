use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let target = std::env::var("TARGET").unwrap_or_else(|_| "unknown-target".to_string());
    println!("cargo:rustc-env=VO_TARGET_TRIPLE={}", target);
    println!("cargo:rerun-if-env-changed=TARGET");

    let manifest_dir = PathBuf::from(
        std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR should be set"),
    );
    let workspace_root = manifest_dir
        .ancestors()
        .nth(3)
        .expect("vo-engine crate should live under <workspace>/lang/crates/vo-engine")
        .to_path_buf();
    let inputs = collect_compiler_identity_inputs(&workspace_root);
    let build_id = compute_build_id(&workspace_root, &inputs);
    println!("cargo:rustc-env=VO_COMPILER_BUILD_ID={}", build_id);
    for path in &inputs {
        println!("cargo:rerun-if-changed={}", path.display());
    }
}

fn collect_compiler_identity_inputs(workspace_root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_identity_files(&workspace_root.join("Cargo.toml"), &mut files);
    collect_identity_files(&workspace_root.join("Cargo.lock"), &mut files);
    collect_identity_files(&workspace_root.join("lang").join("crates"), &mut files);
    collect_identity_files(&workspace_root.join("lang").join("stdlib"), &mut files);
    files.sort();
    files
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
    if matches!(path.file_name().and_then(|name| name.to_str()), Some("Cargo.toml") | Some("Cargo.lock") | Some("build.rs") | Some("stdlib.toml")) {
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
