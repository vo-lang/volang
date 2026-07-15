//! Build script for deterministic Studio WASM build metadata.

use std::{
    env, fs,
    path::Path,
    time::{SystemTime, UNIX_EPOCH},
};

fn studio_wasm_build_id() -> String {
    if let Ok(value) = env::var("VO_STUDIO_BUILD_ID") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            return validate_build_id(trimmed, "VO_STUDIO_BUILD_ID");
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
        return validate_build_id(&github_parts.join("-"), "derived GitHub Studio build ID");
    }
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_millis();
    format!("local-{:x}", now)
}

fn validate_build_id(value: &str, label: &str) -> String {
    if value.is_empty()
        || value.len() > 256
        || !value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'_' | b'-'))
    {
        panic!("{label} must contain 1 to 256 ASCII letters, digits, '.', '_' or '-'");
    }
    value.to_string()
}

fn write_studio_wasm_build_info(out_dir: &str) {
    println!("cargo:rerun-if-env-changed=VO_STUDIO_BUILD_ID");
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

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    write_studio_wasm_build_info(&out_dir);
}
