use std::io::Write;
use std::path::Path;

fn main() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let vogui_root = Path::new(&manifest).join("../../libs/vogui");

    // Tell cargo to re-run if any vogui .vo file changes
    println!("cargo:rerun-if-changed={}", vogui_root.display());

    let out_dir = std::env::var("OUT_DIR").unwrap();
    let out_path = Path::new(&out_dir).join("vogui_files.rs");
    let mut out = std::fs::File::create(&out_path).unwrap();

    // Collect only .vo files, skip js/ and rust/ directories
    let mut entries: Vec<(String, String)> = Vec::new();
    collect_vo_files(&vogui_root, &vogui_root, "vogui", &mut entries);
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    writeln!(out, "static VOGUI_FILES: &[(&str, &[u8])] = &[").unwrap();
    for (vfs_path, abs_path) in &entries {
        writeln!(
            out,
            "    ({:?}, include_bytes!({:?})),",
            vfs_path, abs_path
        )
        .unwrap();
    }
    writeln!(out, "];").unwrap();
}

fn collect_vo_files(root: &Path, dir: &Path, vfs_prefix: &str, out: &mut Vec<(String, String)>) {
    let skip_dirs = ["js", "rust", ".vo-cache", "target", "node_modules"];
    let rd = match std::fs::read_dir(dir) {
        Ok(r) => r,
        Err(_) => return,
    };
    for entry in rd.flatten() {
        let path = entry.path();
        let name = path.file_name().unwrap_or_default().to_str().unwrap_or_default();
        if path.is_dir() {
            if skip_dirs.contains(&name) {
                continue;
            }
            let sub_prefix = format!("{}/{}", vfs_prefix, name);
            collect_vo_files(root, &path, &sub_prefix, out);
        } else if name.ends_with(".vo") {
            let vfs_path = format!("{}/{}", vfs_prefix, name);
            let abs = path.canonicalize().unwrap();
            out.push((vfs_path, abs.to_str().unwrap().to_string()));
        }
    }
}
