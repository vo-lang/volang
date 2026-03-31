pub mod cache;
pub mod compat;
pub mod digest;
pub mod ext_manifest;
#[cfg(not(target_arch = "wasm32"))]
pub mod github_registry;
pub mod identity;
pub mod lock;
pub mod ops;
pub mod project;
pub mod registry;
pub mod schema;
pub mod solver;
pub mod version;
pub mod workspace;

use std::fmt;

#[derive(Debug)]
pub enum Error {
    // Core validation
    InvalidModulePath(String),
    InvalidVersion(String),
    InvalidConstraint(String),
    InvalidDigest(String),
    InvalidImportPath(String),

    // Schema parsing
    ModFileParse(String),
    LockFileParse(String),
    WorkFileParse(String),
    ManifestParse(String),
    ExtManifestParse(String),
    SourceScan(String),

    // Registry
    RegistryError(String),
    InvalidReleaseMetadata(String),

    // Solver
    NoSatisfyingVersion {
        module: String,
        detail: String,
    },
    ConflictingConstraints {
        module: String,
        detail: String,
    },
    DependencyToolchainMismatch {
        module: String,
        project_constraint: String,
        dependency_constraint: String,
    },

    // Lock authority
    RootMismatch {
        field: String,
        mod_value: String,
        lock_value: String,
    },
    LockedModuleMismatch {
        module: String,
        field: String,
        expected: String,
        found: String,
    },

    // Materialization
    DigestMismatch {
        context: String,
        expected: String,
        found: String,
    },
    MissingLockFile,
    MissingArtifact {
        module: String,
        version: String,
        detail: String,
    },

    // Workspace
    WorkspaceIdentityMismatch {
        expected: String,
        found: String,
        path: String,
    },
    OverrideUnlockedDep {
        importer: String,
        import_path: String,
    },
    SelfOverride,

    // IO
    Io(std::io::Error),

    // Network
    Network(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidModulePath(msg) => write!(f, "invalid module path: {msg}"),
            Self::InvalidVersion(msg) => write!(f, "invalid version: {msg}"),
            Self::InvalidConstraint(msg) => write!(f, "invalid constraint: {msg}"),
            Self::InvalidDigest(msg) => write!(f, "invalid digest: {msg}"),
            Self::InvalidImportPath(msg) => write!(f, "invalid import path: {msg}"),
            Self::ModFileParse(msg) => write!(f, "vo.mod parse error: {msg}"),
            Self::LockFileParse(msg) => write!(f, "vo.lock parse error: {msg}"),
            Self::WorkFileParse(msg) => write!(f, "vo.work parse error: {msg}"),
            Self::ManifestParse(msg) => write!(f, "vo.release.json parse error: {msg}"),
            Self::ExtManifestParse(msg) => write!(f, "vo.ext.toml parse error: {msg}"),
            Self::SourceScan(msg) => write!(f, "source scan error: {msg}"),
            Self::RegistryError(msg) => write!(f, "registry error: {msg}"),
            Self::InvalidReleaseMetadata(msg) => write!(f, "invalid release metadata: {msg}"),
            Self::NoSatisfyingVersion { module, detail } => {
                write!(
                    f,
                    "no version of {module} satisfies all constraints: {detail}"
                )
            }
            Self::ConflictingConstraints { module, detail } => {
                write!(f, "conflicting constraints for {module}: {detail}")
            }
            Self::DependencyToolchainMismatch {
                module,
                project_constraint,
                dependency_constraint,
            } => {
                write!(f, "dependency {module} requires toolchain {dependency_constraint} which is incompatible with project constraint {project_constraint}")
            }
            Self::RootMismatch {
                field,
                mod_value,
                lock_value,
            } => {
                write!(f, "root vo.mod vs vo.lock mismatch on {field}: vo.mod has {mod_value}, vo.lock has {lock_value}")
            }
            Self::LockedModuleMismatch {
                module,
                field,
                expected,
                found,
            } => {
                write!(f, "vo.lock entry for {module} does not match published release manifest: {field}: expected {expected}, found {found}")
            }
            Self::DigestMismatch {
                context,
                expected,
                found,
            } => {
                write!(
                    f,
                    "digest mismatch for {context}: expected {expected}, found {found}"
                )
            }
            Self::MissingLockFile => {
                write!(f, "this build imports external modules but vo.lock is missing\n  run: vo mod sync")
            }
            Self::MissingArtifact {
                module,
                version,
                detail,
            } => {
                write!(f, "required locked artifact is missing from cache: {module} {version}: {detail}\n  run: vo mod download")
            }
            Self::WorkspaceIdentityMismatch {
                expected,
                found,
                path,
            } => {
                write!(f, "workspace override identity mismatch at {path}: expected module {expected}, found {found}")
            }
            Self::OverrideUnlockedDep {
                importer,
                import_path,
            } => {
                write!(f, "workspace override imports an external module not in root lockfile: {importer} imports {import_path}\n  run: vo mod sync or disable vo.work")
            }
            Self::SelfOverride => {
                write!(f, "root module must not override itself via vo.work")
            }
            Self::Io(e) => write!(f, "io error: {e}"),
            Self::Network(msg) => write!(f, "network error: {msg}"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

pub type Result<T> = std::result::Result<T, Error>;
