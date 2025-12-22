//! `gox get` command - Download a dependency and add it to gox.mod.

use std::env;
use gox_module::ModFile;

/// Download a dependency and add it to gox.mod.
///
/// # Arguments
/// * `module_version` - Module and version in format `module@version`
///
/// # Examples
/// ```text
/// gox get github.com/foo/bar@v1.2.3
/// ```
pub fn run(module_version: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Parse module@version
    let parts: Vec<&str> = module_version.splitn(2, '@').collect();
    if parts.len() != 2 {
        return Err(format!(
            "invalid format: expected <module>@<version>, got: {}",
            module_version
        ).into());
    }

    let module = parts[0];
    let version = parts[1];

    if !version.starts_with('v') {
        return Err(format!("version must start with 'v', got: {}", version).into());
    }

    let cwd = env::current_dir()?;
    let mod_file_path = cwd.join("gox.mod");

    // Load existing gox.mod
    let mut mod_file = ModFile::parse_file(&mod_file_path)?;

    // Create .goxdeps directory if needed
    let deps_dir = cwd.join(".goxdeps");
    std::fs::create_dir_all(&deps_dir)?;

    // TODO: Actually download the module from a registry
    // For now, just create a placeholder directory structure
    let module_dir = deps_dir.join(format!("{}@{}", module, version));
    if !module_dir.exists() {
        std::fs::create_dir_all(&module_dir)?;
        
        // Create a minimal gox.mod for the dependency
        let dep_mod = ModFile::new(module.to_string());
        dep_mod.write_file(module_dir.join("gox.mod"))?;
        
        println!("Created placeholder for {}@{}", module, version);
        println!("  (actual download not implemented yet)");
    }

    // Generate alias from module path (use last path component)
    // e.g., "github.com/gin-gonic/gin" -> "gin"
    let alias = module
        .rsplit('/')
        .next()
        .unwrap_or(module)
        .to_string();

    // Add to gox.mod
    mod_file.add_require(alias.clone(), module.to_string(), version.to_string());
    mod_file.write_file(&mod_file_path)?;

    println!("Added {} = {}@{} to gox.mod", alias, module, version);
    Ok(())
}
