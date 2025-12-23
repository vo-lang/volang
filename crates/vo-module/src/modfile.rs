//! Parser for vo.mod files.
//!
//! Format:
//! ```text
//! module <module-path>
//!
//! require <alias> <module-path> <version>
//! require <alias> <module-path> <version>
//! ```
//!
//! The alias is used in source code with `@"alias"` syntax for external imports.

use std::fs;
use std::path::Path;

use crate::error::{ModuleError, ModuleResult};

/// A parsed vo.mod file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModFile {
    /// The module path (e.g., "github.com/myuser/myproject").
    pub module: String,

    /// Direct dependencies.
    pub requires: Vec<Require>,
}

/// A single require directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Require {
    /// The alias used in source code with @"alias" syntax.
    pub alias: String,

    /// The module path (e.g., "github.com/gin-gonic/gin").
    pub module: String,

    /// The exact version (e.g., "v1.2.3").
    pub version: String,
}

impl ModFile {
    /// Parses a vo.mod file from the given path.
    pub fn parse_file<P: AsRef<Path>>(path: P) -> ModuleResult<Self> {
        let path = path.as_ref();
        if !path.exists() {
            return Err(ModuleError::ModFileNotFound(path.to_path_buf()));
        }

        let content = fs::read_to_string(path)
            .map_err(|e| ModuleError::IoError(path.to_path_buf(), e.to_string()))?;

        Self::parse(&content, path)
    }

    /// Parses vo.mod content from a string.
    pub fn parse(content: &str, file_path: &Path) -> ModuleResult<Self> {
        let mut module: Option<String> = None;
        let mut requires: Vec<Require> = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let line_num = line_num + 1; // 1-indexed
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with("//") {
                continue;
            }

            // Parse module declaration
            if line.starts_with("module ") {
                if module.is_some() {
                    return Err(ModuleError::DuplicateModuleDecl(file_path.to_path_buf()));
                }

                let module_path = line["module ".len()..].trim();
                if !is_valid_module_path(module_path) {
                    return Err(ModuleError::ParseError {
                        file: file_path.to_path_buf(),
                        line: line_num,
                        message: format!("invalid module path: {}", module_path),
                    });
                }

                module = Some(module_path.to_string());
                continue;
            }

            // Parse require directive
            if line.starts_with("require ") {
                let rest = line["require ".len()..].trim();
                let parts: Vec<&str> = rest.split_whitespace().collect();

                if parts.len() != 3 {
                    return Err(ModuleError::ParseError {
                        file: file_path.to_path_buf(),
                        line: line_num,
                        message: format!(
                            "invalid require syntax, expected: require <alias> <module> <version>, got: {}",
                            line
                        ),
                    });
                }

                let req_alias = parts[0];
                let req_module = parts[1];
                let req_version = parts[2];

                if !is_valid_alias(req_alias) {
                    return Err(ModuleError::ParseError {
                        file: file_path.to_path_buf(),
                        line: line_num,
                        message: format!("invalid alias: {}", req_alias),
                    });
                }

                if !is_valid_module_path(req_module) {
                    return Err(ModuleError::ParseError {
                        file: file_path.to_path_buf(),
                        line: line_num,
                        message: format!("invalid module path: {}", req_module),
                    });
                }

                if !is_valid_version(req_version) {
                    return Err(ModuleError::ParseError {
                        file: file_path.to_path_buf(),
                        line: line_num,
                        message: format!("invalid version: {}", req_version),
                    });
                }

                requires.push(Require {
                    alias: req_alias.to_string(),
                    module: req_module.to_string(),
                    version: req_version.to_string(),
                });
                continue;
            }

            // Unknown directive
            return Err(ModuleError::ParseError {
                file: file_path.to_path_buf(),
                line: line_num,
                message: format!("unknown directive: {}", line),
            });
        }

        let module = module.ok_or_else(|| ModuleError::MissingModuleDecl(file_path.to_path_buf()))?;

        Ok(ModFile { module, requires })
    }

    /// Creates a new empty ModFile with the given module path.
    pub fn new(module: String) -> Self {
        ModFile {
            module,
            requires: Vec::new(),
        }
    }

    /// Adds a require directive.
    pub fn add_require(&mut self, alias: String, module: String, version: String) {
        // Check if alias already exists and update
        for req in &mut self.requires {
            if req.alias == alias {
                req.module = module;
                req.version = version;
                return;
            }
        }
        self.requires.push(Require { alias, module, version });
    }

    /// Finds a require by alias.
    pub fn find_by_alias(&self, alias: &str) -> Option<&Require> {
        self.requires.iter().find(|r| r.alias == alias)
    }

    /// Serializes the ModFile to a string.
    pub fn to_string(&self) -> String {
        let mut result = format!("module {}\n", self.module);

        if !self.requires.is_empty() {
            result.push('\n');
            for req in &self.requires {
                result.push_str(&format!("require {} {} {}\n", req.alias, req.module, req.version));
            }
        }

        result
    }

    /// Writes the ModFile to a file.
    pub fn write_file<P: AsRef<Path>>(&self, path: P) -> ModuleResult<()> {
        let path = path.as_ref();
        fs::write(path, self.to_string())
            .map_err(|e| ModuleError::IoError(path.to_path_buf(), e.to_string()))
    }
}

/// Validates an alias.
///
/// A valid alias:
/// - Is not empty
/// - Starts with a letter or underscore
/// - Contains only letters, digits, and underscores
fn is_valid_alias(alias: &str) -> bool {
    if alias.is_empty() {
        return false;
    }
    let mut chars = alias.chars();
    let first = chars.next().unwrap();
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Validates a module path.
///
/// A valid module path:
/// - Is not empty
/// - Does not start or end with /
/// - Does not contain //
/// - Does not start with std/ (reserved for standard library)
fn is_valid_module_path(path: &str) -> bool {
    if path.is_empty() {
        return false;
    }
    if path.starts_with('/') || path.ends_with('/') {
        return false;
    }
    if path.contains("//") {
        return false;
    }
    if path.starts_with("std/") || path == "std" {
        return false;
    }
    // Must contain at least one path component
    true
}

/// Validates a version string.
///
/// A valid version:
/// - Starts with 'v'
/// - Has format vMAJOR.MINOR.PATCH with optional pre-release/build metadata
fn is_valid_version(version: &str) -> bool {
    if !version.starts_with('v') {
        return false;
    }

    let rest = &version[1..];
    
    // Split off pre-release (-) and build metadata (+)
    let version_core = rest
        .split('-')
        .next()
        .unwrap_or("")
        .split('+')
        .next()
        .unwrap_or("");

    // Check MAJOR.MINOR.PATCH format
    let parts: Vec<&str> = version_core.split('.').collect();
    if parts.len() < 2 {
        return false;
    }

    // All parts must be numeric
    for part in &parts {
        if part.is_empty() || !part.chars().all(|c| c.is_ascii_digit()) {
            return false;
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_parse_simple() {
        let content = r#"
module github.com/myuser/myproject

require bar github.com/foo/bar v1.2.3
require qux github.com/baz/qux v0.1.0
"#;
        let mod_file = ModFile::parse(content, &PathBuf::from("vo.mod")).unwrap();
        
        assert_eq!(mod_file.module, "github.com/myuser/myproject");
        assert_eq!(mod_file.requires.len(), 2);
        assert_eq!(mod_file.requires[0].alias, "bar");
        assert_eq!(mod_file.requires[0].module, "github.com/foo/bar");
        assert_eq!(mod_file.requires[0].version, "v1.2.3");
        assert_eq!(mod_file.requires[1].alias, "qux");
        assert_eq!(mod_file.requires[1].module, "github.com/baz/qux");
        assert_eq!(mod_file.requires[1].version, "v0.1.0");
    }

    #[test]
    fn test_parse_with_comments() {
        let content = r#"
// This is a comment
module github.com/myuser/myproject

// Another comment
require bar github.com/foo/bar v1.2.3
"#;
        let mod_file = ModFile::parse(content, &PathBuf::from("vo.mod")).unwrap();
        
        assert_eq!(mod_file.module, "github.com/myuser/myproject");
        assert_eq!(mod_file.requires.len(), 1);
        assert_eq!(mod_file.requires[0].alias, "bar");
    }

    #[test]
    fn test_parse_no_requires() {
        let content = "module myproject\n";
        let mod_file = ModFile::parse(content, &PathBuf::from("vo.mod")).unwrap();
        
        assert_eq!(mod_file.module, "myproject");
        assert!(mod_file.requires.is_empty());
    }

    #[test]
    fn test_parse_missing_module() {
        let content = "require bar github.com/foo/bar v1.0.0\n";
        let result = ModFile::parse(content, &PathBuf::from("vo.mod"));
        
        assert!(matches!(result, Err(ModuleError::MissingModuleDecl(_))));
    }

    #[test]
    fn test_parse_duplicate_module() {
        let content = r#"
module github.com/a
module github.com/b
"#;
        let result = ModFile::parse(content, &PathBuf::from("vo.mod"));
        
        assert!(matches!(result, Err(ModuleError::DuplicateModuleDecl(_))));
    }

    #[test]
    fn test_parse_invalid_version() {
        let content = r#"
module myproject
require bar github.com/foo/bar 1.2.3
"#;
        let result = ModFile::parse(content, &PathBuf::from("vo.mod"));
        
        assert!(matches!(result, Err(ModuleError::ParseError { .. })));
    }

    #[test]
    fn test_parse_std_module_rejected() {
        let content = "module std/io\n";
        let result = ModFile::parse(content, &PathBuf::from("vo.mod"));
        
        assert!(matches!(result, Err(ModuleError::ParseError { .. })));
    }

    #[test]
    fn test_valid_versions() {
        assert!(is_valid_version("v1.0.0"));
        assert!(is_valid_version("v0.1.0"));
        assert!(is_valid_version("v2.0.0-beta.1"));
        assert!(is_valid_version("v1.2.3+meta"));
        assert!(is_valid_version("v1.2.3-rc.1+build.123"));
        assert!(is_valid_version("v1.0"));
        
        assert!(!is_valid_version("1.0.0"));
        assert!(!is_valid_version("v"));
        assert!(!is_valid_version(""));
        assert!(!is_valid_version("v.1.0"));
    }

    #[test]
    fn test_to_string() {
        let mod_file = ModFile {
            module: "github.com/myuser/myproject".to_string(),
            requires: vec![
                Require {
                    alias: "bar".to_string(),
                    module: "github.com/foo/bar".to_string(),
                    version: "v1.2.3".to_string(),
                },
            ],
        };

        let expected = r#"module github.com/myuser/myproject

require bar github.com/foo/bar v1.2.3
"#;
        assert_eq!(mod_file.to_string(), expected);
    }

    #[test]
    fn test_valid_aliases() {
        assert!(is_valid_alias("foo"));
        assert!(is_valid_alias("_foo"));
        assert!(is_valid_alias("foo123"));
        assert!(is_valid_alias("foo_bar"));
        
        assert!(!is_valid_alias(""));
        assert!(!is_valid_alias("123foo"));
        assert!(!is_valid_alias("foo-bar"));
        assert!(!is_valid_alias("foo.bar"));
    }

    #[test]
    fn test_find_by_alias() {
        let mod_file = ModFile {
            module: "myproject".to_string(),
            requires: vec![
                Require {
                    alias: "gin".to_string(),
                    module: "github.com/gin-gonic/gin".to_string(),
                    version: "v1.9.0".to_string(),
                },
            ],
        };

        assert!(mod_file.find_by_alias("gin").is_some());
        assert!(mod_file.find_by_alias("unknown").is_none());
    }

    #[test]
    fn test_valid_module_paths() {
        // Valid paths
        assert!(is_valid_module_path("myproject"));
        assert!(is_valid_module_path("github.com/user/repo"));
        assert!(is_valid_module_path("github.com/user/repo/pkg"));
        assert!(is_valid_module_path("example.com/foo"));
        
        // Invalid paths
        assert!(!is_valid_module_path(""));
        assert!(!is_valid_module_path("/foo"));
        assert!(!is_valid_module_path("foo/"));
        assert!(!is_valid_module_path("foo//bar"));
        assert!(!is_valid_module_path("std"));
        assert!(!is_valid_module_path("std/io"));
        assert!(!is_valid_module_path("std/fmt"));
    }

    #[test]
    fn test_add_require() {
        let mut mod_file = ModFile::new("myproject".to_string());
        
        // Add first require
        mod_file.add_require("gin".to_string(), "github.com/gin-gonic/gin".to_string(), "v1.9.0".to_string());
        assert_eq!(mod_file.requires.len(), 1);
        assert_eq!(mod_file.requires[0].alias, "gin");
        assert_eq!(mod_file.requires[0].version, "v1.9.0");
        
        // Update existing alias
        mod_file.add_require("gin".to_string(), "github.com/gin-gonic/gin".to_string(), "v1.10.0".to_string());
        assert_eq!(mod_file.requires.len(), 1);
        assert_eq!(mod_file.requires[0].version, "v1.10.0");
        
        // Add different alias
        mod_file.add_require("jwt".to_string(), "github.com/golang-jwt/jwt".to_string(), "v5.0.0".to_string());
        assert_eq!(mod_file.requires.len(), 2);
    }

    #[test]
    fn test_parse_invalid_alias() {
        let content = r#"
module myproject
require 123invalid github.com/foo/bar v1.0.0
"#;
        let result = ModFile::parse(content, &PathBuf::from("vo.mod"));
        assert!(matches!(result, Err(ModuleError::ParseError { .. })));
    }

    #[test]
    fn test_parse_invalid_module_path() {
        let content = r#"
module myproject
require foo std/io v1.0.0
"#;
        let result = ModFile::parse(content, &PathBuf::from("vo.mod"));
        assert!(matches!(result, Err(ModuleError::ParseError { .. })));
    }

    #[test]
    fn test_parse_file_and_write_file() {
        use std::fs;
        
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("test_vo.mod");
        
        // Write a vo.mod file
        let content = r#"module github.com/test/project

require gin github.com/gin-gonic/gin v1.9.0
require jwt github.com/golang-jwt/jwt v5.0.0
"#;
        fs::write(&test_file, content).unwrap();
        
        // Parse the file
        let mod_file = ModFile::parse_file(&test_file).unwrap();
        assert_eq!(mod_file.module, "github.com/test/project");
        assert_eq!(mod_file.requires.len(), 2);
        assert_eq!(mod_file.requires[0].alias, "gin");
        assert_eq!(mod_file.requires[1].alias, "jwt");
        
        // Modify and write back
        let mut mod_file = mod_file;
        mod_file.add_require("echo".to_string(), "github.com/labstack/echo".to_string(), "v4.11.0".to_string());
        
        let output_file = temp_dir.join("test_vo_output.mod");
        mod_file.write_file(&output_file).unwrap();
        
        // Re-read and verify
        let mod_file2 = ModFile::parse_file(&output_file).unwrap();
        assert_eq!(mod_file2.requires.len(), 3);
        assert!(mod_file2.find_by_alias("echo").is_some());
        
        // Cleanup
        let _ = fs::remove_file(&test_file);
        let _ = fs::remove_file(&output_file);
    }

    #[test]
    fn test_parse_file_not_found() {
        let result = ModFile::parse_file("/nonexistent/path/vo.mod");
        assert!(matches!(result, Err(ModuleError::ModFileNotFound(_))));
    }

    #[test]
    fn test_roundtrip() {
        let original = ModFile {
            module: "github.com/user/project".to_string(),
            requires: vec![
                Require {
                    alias: "foo".to_string(),
                    module: "github.com/foo/foo".to_string(),
                    version: "v1.0.0".to_string(),
                },
                Require {
                    alias: "bar".to_string(),
                    module: "github.com/bar/bar".to_string(),
                    version: "v2.0.0-beta.1".to_string(),
                },
            ],
        };
        
        // Serialize to string
        let serialized = original.to_string();
        
        // Parse back
        let parsed = ModFile::parse(&serialized, &PathBuf::from("vo.mod")).unwrap();
        
        // Verify equality
        assert_eq!(parsed.module, original.module);
        assert_eq!(parsed.requires.len(), original.requires.len());
        for (p, o) in parsed.requires.iter().zip(original.requires.iter()) {
            assert_eq!(p.alias, o.alias);
            assert_eq!(p.module, o.module);
            assert_eq!(p.version, o.version);
        }
    }
}
