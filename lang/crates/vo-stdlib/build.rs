use std::path::Path;

fn main() {
    // Tell cargo to rerun this build script if any .vo file in stdlib changes
    let stdlib_dir = Path::new("../../stdlib");
    println!("cargo:rerun-if-changed={}", stdlib_dir.display());
    
    // Walk the directory and watch all .vo files
    if let Ok(entries) = walkdir(stdlib_dir) {
        for entry in entries {
            println!("cargo:rerun-if-changed={}", entry.display());
        }
    }
}

fn walkdir(dir: &Path) -> std::io::Result<Vec<std::path::PathBuf>> {
    let mut files = Vec::new();
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                files.extend(walkdir(&path)?);
            } else if path.extension().map_or(false, |ext| ext == "vo" || ext == "toml") {
                files.push(path);
            }
        }
    }
    Ok(files)
}
