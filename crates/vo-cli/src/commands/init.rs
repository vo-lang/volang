//! `vo init` command - Initialize a new Vo module.

use std::env;
use vo_module::ModFile;

/// Initialize a new Vo module in the current directory.
///
/// # Arguments
/// * `module_path` - Module path (e.g., github.com/user/project)
///
/// # Examples
/// ```text
/// vo init github.com/user/myapp
/// ```
pub fn run(module_path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let cwd = env::current_dir()?;
    let mod_file_path = cwd.join("vo.mod");

    if mod_file_path.exists() {
        return Err(format!("vo.mod already exists in {}", cwd.display()).into());
    }

    // Validate module path
    if module_path.is_empty() {
        return Err("module path cannot be empty".into());
    }
    if module_path.starts_with("std/") || module_path == "std" {
        return Err("module path cannot start with 'std/' (reserved for standard library)".into());
    }

    let mod_file = ModFile::new(module_path.to_string());
    mod_file.write_file(&mod_file_path)?;

    println!("Initialized module {} in {}", module_path, cwd.display());
    Ok(())
}
