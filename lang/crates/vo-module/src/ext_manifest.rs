//! Extension manifest discovery.
//!
//! Discovers native extension manifests (`vo.ext.toml`) from package directories.

use std::path::{Path, PathBuf};

/// Parsed extension manifest from `vo.ext.toml`.
#[derive(Debug, Clone)]
pub struct ExtensionManifest {
    /// Extension name.
    pub name: String,
    /// Path to native library.
    pub native_path: PathBuf,
}

/// Error type for extension manifest parsing.
#[derive(Debug)]
pub enum ExtManifestError {
    /// Manifest parse error.
    Parse(String),
    /// IO error.
    Io(std::io::Error),
}

impl std::fmt::Display for ExtManifestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExtManifestError::Parse(msg) => write!(f, "manifest error: {}", msg),
            ExtManifestError::Io(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl std::error::Error for ExtManifestError {}

impl From<std::io::Error> for ExtManifestError {
    fn from(e: std::io::Error) -> Self {
        ExtManifestError::Io(e)
    }
}

/// Discover extension manifests from a package directory.
///
/// Looks for `vo.ext.toml` files and returns parsed manifests.
pub fn discover_extensions(pkg_root: &Path) -> Result<Vec<ExtensionManifest>, ExtManifestError> {
    let manifest_path = pkg_root.join("vo.ext.toml");
    if !manifest_path.exists() {
        return Ok(Vec::new());
    }

    let manifest = parse_manifest(&manifest_path)?;
    Ok(vec![manifest])
}

/// Parse a vo.ext.toml manifest file.
fn parse_manifest(path: &Path) -> Result<ExtensionManifest, ExtManifestError> {
    let content = std::fs::read_to_string(path)?;
    
    let mut name = String::new();
    let mut native_path = String::new();

    for line in content.lines() {
        let line = line.trim();
        if line.starts_with("name") {
            if let Some(val) = extract_toml_string(line) {
                name = val;
            }
        } else if line.starts_with("path") {
            if let Some(val) = extract_toml_string(line) {
                native_path = val;
            }
        }
    }

    if name.is_empty() {
        return Err(ExtManifestError::Parse("missing 'name' in [extension]".to_string()));
    }
    if native_path.is_empty() {
        return Err(ExtManifestError::Parse("missing 'path' in [native]".to_string()));
    }

    let parent = path.parent().unwrap_or(Path::new("."));
    let full_path = parent.join(&native_path);
    
    // Auto-append platform-specific extension if not present
    let full_path = resolve_library_path(full_path);

    Ok(ExtensionManifest {
        name,
        native_path: full_path,
    })
}

/// Resolve library path with platform-specific extension and profile.
///
/// - Replaces `{profile}` with the Cargo build profile (`debug`, `release`,
///   `release-native`, …) captured at compile time via `build.rs`.
/// - If the path doesn't have a known library extension (.so, .dylib, .dll),
///   automatically append the correct one for the current platform.
fn resolve_library_path(path: PathBuf) -> PathBuf {
    // Replace {profile} placeholder with the actual Cargo profile name embedded
    // by build.rs.  This correctly distinguishes debug / release / release-native.
    let path_str = path.to_string_lossy();
    let path = if path_str.contains("{profile}") {
        PathBuf::from(path_str.replace("{profile}", env!("VO_BUILD_PROFILE")))
    } else {
        path
    };
    
    // Check if already has a library extension
    if let Some(ext) = path.extension() {
        let ext = ext.to_string_lossy();
        if ext == "so" || ext == "dylib" || ext == "dll" {
            return path;
        }
    }
    
    // Append platform-specific extension
    #[cfg(target_os = "linux")]
    {
        path.with_extension("so")
    }
    #[cfg(target_os = "macos")]
    {
        path.with_extension("dylib")
    }
    #[cfg(target_os = "windows")]
    {
        // Windows: libfoo -> foo.dll
        let file_name = path.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");
        let new_name = if file_name.starts_with("lib") {
            &file_name[3..]
        } else {
            file_name
        };
        path.with_file_name(new_name).with_extension("dll")
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        // Fallback: try .so
        path.with_extension("so")
    }
}

/// Extract a string value from a TOML line like `key = "value"`.
fn extract_toml_string(line: &str) -> Option<String> {
    let parts: Vec<&str> = line.splitn(2, '=').collect();
    if parts.len() != 2 {
        return None;
    }
    let val = parts[1].trim();
    if val.starts_with('"') && val.ends_with('"') && val.len() >= 2 {
        Some(val[1..val.len()-1].to_string())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_toml_string() {
        assert_eq!(extract_toml_string(r#"name = "test""#), Some("test".to_string()));
        assert_eq!(extract_toml_string(r#"path = "native/lib.so""#), Some("native/lib.so".to_string()));
        assert_eq!(extract_toml_string("invalid"), None);
    }
}
