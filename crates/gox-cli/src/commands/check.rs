//! `gox check` command - Type-check the current module without building.

use std::env;
use gox_module::{ModFile, ModuleResolver};

/// Type-check the current module without building.
///
/// # Examples
/// ```text
/// gox check
/// ```
pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = env::current_dir()?;
    let mod_file_path = cwd.join("gox.mod");

    // Load gox.mod
    let mod_file = ModFile::parse_file(&mod_file_path)?;
    println!("Checking module: {}", mod_file.module);

    // Create resolver and compute closure
    let resolver = ModuleResolver::new(&cwd);
    let _closure = resolver.compute_closure(&mod_file)?;

    // TODO: Find all .gox files and type-check them
    println!("\nType checking not implemented yet");
    Ok(())
}
