//! Build script: embed vogui .vo source files for WASM user-code compilation.
//!
//! Embeds all top-level .vo files from libs/vogui/ under the "vogui/" VFS prefix,
//! so that user code with `import "vogui"` can be compiled inside the browser.

use std::{env, fs, path::{Path, PathBuf}};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest = Path::new(&out_dir).join("vogui_embedded.rs");
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    // studio/wasm -> studio -> repo root
    let repo_root = manifest_dir.join("../..").canonicalize().unwrap();
    let vogui_dir = repo_root.join("libs/vogui");

    let mut entries = String::new();
    if let Ok(rd) = fs::read_dir(&vogui_dir) {
        let mut paths: Vec<_> = rd.flatten()
            .map(|e| e.path())
            .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("vo"))
            .collect();
        paths.sort();
        for path in paths {
            let name = path.file_name().unwrap().to_str().unwrap();
            entries.push_str(&format!(
                "    ({:?}, include_bytes!({:?})),\n",
                format!("vogui/{}", name),
                path.display()
            ));
        }
    }

    fs::write(
        &dest,
        format!("pub static VOGUI_FILES: &[(&str, &[u8])] = &[\n{}];\n", entries),
    ).unwrap();

    println!("cargo:rerun-if-changed={}", vogui_dir.display());
}
