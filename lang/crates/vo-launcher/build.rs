//! Build script for vo-launcher.
//!
//! Ensures recompilation when stdlib .vo files change.

use std::fs;
use std::path::Path;

fn main() {
    // Watch stdlib directory for changes
    let stdlib_dir = Path::new("../../stdlib");
    if stdlib_dir.exists() {
        watch_dir_recursive(stdlib_dir);
    }
}

fn watch_dir_recursive(dir: &Path) {
    println!("cargo:rerun-if-changed={}", dir.display());
    
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                watch_dir_recursive(&path);
            } else if path.extension().map(|e| e == "vo").unwrap_or(false) {
                println!("cargo:rerun-if-changed={}", path.display());
            }
        }
    }
}
