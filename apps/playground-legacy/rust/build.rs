use std::io::Write;
use std::path::{Path, PathBuf};

mod build_support;
use build_support::{canonical_regular_file, collect_vo_files};

fn main() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let repo_root = Path::new(&manifest).join("../../..");
    let vogui_root = std::env::var_os("VOGUI_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| repo_root.join("ci_modules/vogui"));
    let (vogui_root, entries) = collect_vo_files(&vogui_root)
        .unwrap_or_else(|error| panic!("cannot collect embedded vogui source: {error}"));
    let vogui_mod = vogui_root.join("vo.mod");
    let vogui_mod = canonical_regular_file(&vogui_root, &vogui_mod, "vogui vo.mod")
        .unwrap_or_else(|error| panic!("cannot embed vogui metadata: {error}"));

    let out_dir = std::env::var("OUT_DIR").unwrap();
    let out_path = Path::new(&out_dir).join("vogui_files.rs");
    let mut out = std::fs::File::create(&out_path).unwrap();

    println!("cargo:rerun-if-env-changed=VOGUI_ROOT");
    println!("cargo:rerun-if-changed={}", vogui_root.display());
    println!("cargo:rerun-if-changed={}", vogui_mod.display());
    for entry in &entries {
        println!("cargo:rerun-if-changed={}", entry.absolute_path.display());
    }

    writeln!(
        out,
        "static VOGUI_MOD_FILE: &[u8] = include_bytes!({:?});",
        vogui_mod
    )
    .unwrap();
    writeln!(out, "static VOGUI_FILES: &[(&str, &[u8])] = &[").unwrap();
    for entry in &entries {
        writeln!(
            out,
            "    ({:?}, include_bytes!({:?})),",
            entry.vfs_path, entry.absolute_path
        )
        .unwrap();
    }
    writeln!(out, "];").unwrap();
}
