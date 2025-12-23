//! Error types for the module system.

use std::fmt;
use std::path::PathBuf;

/// Result type for module operations.
pub type ModuleResult<T> = Result<T, ModuleError>;

/// Errors that can occur during module operations.
#[derive(Debug, Clone)]
pub enum ModuleError {
    /// vo.mod file not found.
    ModFileNotFound(PathBuf),

    /// Failed to read file.
    IoError(PathBuf, String),

    /// Parse error in vo.mod.
    ParseError {
        file: PathBuf,
        line: usize,
        message: String,
    },

    /// Missing module declaration.
    MissingModuleDecl(PathBuf),

    /// Duplicate module declaration.
    DuplicateModuleDecl(PathBuf),

    /// Invalid module path.
    InvalidModulePath(String),

    /// Invalid version format.
    InvalidVersion(String),

    /// Module not found in .vodeps.
    ModuleNotFound {
        module: String,
        version: String,
    },

    /// Version conflict: same module required at different versions.
    VersionConflict {
        module: String,
        version1: String,
        chain1: Vec<String>,
        version2: String,
        chain2: Vec<String>,
    },

    /// Import path not found in closure.
    UnownedImportPath(String),

    /// Package directory not found.
    PackageNotFound {
        import_path: String,
        expected_dir: PathBuf,
    },

    /// Internal package access violation.
    InternalPackageViolation {
        importer: String,
        internal_pkg: String,
    },

    /// Import cycle detected.
    ImportCycle(Vec<String>),

    /// Standard library package not found.
    StdPackageNotFound(String),
}

impl fmt::Display for ModuleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModuleError::ModFileNotFound(path) => {
                write!(f, "vo.mod not found at {}", path.display())
            }
            ModuleError::IoError(path, msg) => {
                write!(f, "failed to read {}: {}", path.display(), msg)
            }
            ModuleError::ParseError { file, line, message } => {
                write!(f, "{}:{}: {}", file.display(), line, message)
            }
            ModuleError::MissingModuleDecl(path) => {
                write!(f, "missing module declaration in {}", path.display())
            }
            ModuleError::DuplicateModuleDecl(path) => {
                write!(f, "duplicate module declaration in {}", path.display())
            }
            ModuleError::InvalidModulePath(path) => {
                write!(f, "invalid module path: {}", path)
            }
            ModuleError::InvalidVersion(version) => {
                write!(f, "invalid version format: {}", version)
            }
            ModuleError::ModuleNotFound { module, version } => {
                write!(
                    f,
                    "cannot find module {}@{} in .vodeps\n  run: vo get {}@{}",
                    module, version, module, version
                )
            }
            ModuleError::VersionConflict {
                module,
                version1,
                chain1,
                version2,
                chain2,
            } => {
                write!(
                    f,
                    "module {} required at both {} and {}\n  {} required by: {}\n  {} required by: {}",
                    module,
                    version1,
                    version2,
                    version1,
                    chain1.join(" -> "),
                    version2,
                    chain2.join(" -> ")
                )
            }
            ModuleError::UnownedImportPath(path) => {
                write!(
                    f,
                    "import path \"{}\" is not in std/ and matches no module in the closure",
                    path
                )
            }
            ModuleError::PackageNotFound { import_path, expected_dir } => {
                write!(
                    f,
                    "package \"{}\" not found at {}",
                    import_path,
                    expected_dir.display()
                )
            }
            ModuleError::InternalPackageViolation { importer, internal_pkg } => {
                write!(
                    f,
                    "use of internal package not allowed\n  {} cannot import {}",
                    importer, internal_pkg
                )
            }
            ModuleError::ImportCycle(cycle) => {
                write!(f, "import cycle detected\n  {}", cycle.join(" imports\n  "))
            }
            ModuleError::StdPackageNotFound(pkg) => {
                write!(f, "standard library package \"{}\" not found", pkg)
            }
        }
    }
}

impl std::error::Error for ModuleError {}
