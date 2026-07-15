pub mod artifact;
pub mod async_install;
mod async_solver;
pub mod cache;
pub mod digest;
pub mod ephemeral;
pub mod ext_manifest;
#[cfg(all(not(target_arch = "wasm32"), feature = "native-registry"))]
pub mod github_registry;
pub mod identity;
pub mod inline_mod;
pub mod lifecycle;
pub mod lock;
pub mod operation_error;
pub mod ops;
pub mod project;
pub mod readiness;
pub mod registry;
pub mod resolved_extension;
pub mod schema;
pub mod solver;
pub mod version;
pub mod workspace;

use std::fmt;

/// Hard ceiling for a single target artifact materialized by the module system.
pub const MAX_MODULE_ARTIFACT_BYTES: u64 = 512 * 1024 * 1024;
/// Hard ceiling for a compressed module source archive.
pub const MAX_SOURCE_ARCHIVE_BYTES: u64 = 256 * 1024 * 1024;
/// Hard ceiling for entries in one module source archive.
pub const MAX_SOURCE_ARCHIVE_ENTRIES: usize = 100_000;
/// Hard ceiling for one extracted source-archive entry.
pub const MAX_SOURCE_ARCHIVE_ENTRY_BYTES: usize = 64 * 1024 * 1024;
/// Hard ceiling for total extracted bytes in one source archive.
pub const MAX_EXTRACTED_SOURCE_BYTES: usize = 512 * 1024 * 1024;
/// Hard ceiling for direct dependencies declared by one module or manifest.
pub const MAX_MODULE_DEPENDENCIES: usize = 10_000;
/// Hard ceiling for target artifacts declared by one module version.
pub const MAX_MODULE_ARTIFACTS: usize = 10_000;
/// Hard ceiling for one metadata array or map in module manifests.
pub const MAX_MODULE_METADATA_ENTRIES: usize = 10_000;
/// Hard ceiling for release versions retained from one registry listing.
pub const MAX_REGISTRY_RELEASES: usize = 10_000;
/// Hard ceiling for raw registry-index bytes processed while listing one
/// module across all transport pages.
pub const MAX_REGISTRY_LISTING_BYTES: usize = 64 * 1024 * 1024;
/// Hard ceiling for distinct dependency edges in one selected lock/solve graph.
pub const MAX_SOLVER_GRAPH_EDGES: usize = 100_000;
/// Hard ceiling for target artifacts retained across one selected graph.
pub const MAX_SOLVER_GRAPH_ARTIFACTS: usize = 100_000;
/// Hard ceiling for normalized registry candidates retained by one solve.
pub const MAX_SOLVER_CANDIDATES: usize = 100_000;
/// Hard ceiling for raw release-manifest bytes fetched and processed by one solve.
pub const MAX_SOLVER_MANIFEST_BYTES: usize = 256 * 1024 * 1024;
/// Hard ceiling for candidate decisions attempted by one backtracking solve.
pub const MAX_SOLVER_SEARCH_DECISIONS: usize = 100_000;
/// Hard ceiling for failed branches revisited by one backtracking solve.
pub const MAX_SOLVER_BACKTRACKS: usize = 100_000;
/// Hard ceiling for canonical `vo.lock` bytes. This is intentionally larger
/// than the generic source-text limit so a valid 100,000-edge graph with
/// maximum-length module paths and constraints can round-trip.
pub const MAX_LOCK_FILE_BYTES: usize = 128 * 1024 * 1024;

#[derive(Debug)]
pub enum Error {
    // Core validation
    InvalidModulePath(String),
    InvalidVersion(String),
    InvalidConstraint(String),
    InvalidDigest(String),
    InvalidImportPath(String),
    DependencyGraph(String),

    // Schema parsing
    ModFileParse(String),
    LockFileParse(String),
    WorkFileParse(String),
    ManifestParse(String),
    ExtManifestParse(String),
    SourceScan(String),

    // Registry
    RegistryError(String),
    RegistryNotFound {
        resource: String,
    },
    RegistryResponseTooLarge {
        resource: String,
        limit: usize,
    },
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
    SelectedVersionConflict {
        module: String,
        existing: String,
        requested: String,
    },
    DependencyToolchainMismatch {
        module: String,
        project_constraint: String,
        dependency_constraint: String,
    },
    ResolutionLimitExceeded {
        resource: String,
        limit: usize,
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
    MissingArtifact {
        module: String,
        version: String,
        detail: String,
    },
    MissingLockedArtifact {
        module: String,
        version: String,
        detail: String,
    },
    CachePublicationDurabilityUnconfirmed {
        path: String,
        message: String,
    },
    CachePublicationLocationUnconfirmed {
        path: String,
        message: String,
    },
    ProjectPublicationPostCommitFailure {
        path: String,
        message: String,
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

impl Clone for Error {
    fn clone(&self) -> Self {
        match self {
            Self::InvalidModulePath(value) => Self::InvalidModulePath(value.clone()),
            Self::InvalidVersion(value) => Self::InvalidVersion(value.clone()),
            Self::InvalidConstraint(value) => Self::InvalidConstraint(value.clone()),
            Self::InvalidDigest(value) => Self::InvalidDigest(value.clone()),
            Self::InvalidImportPath(value) => Self::InvalidImportPath(value.clone()),
            Self::DependencyGraph(value) => Self::DependencyGraph(value.clone()),
            Self::ModFileParse(value) => Self::ModFileParse(value.clone()),
            Self::LockFileParse(value) => Self::LockFileParse(value.clone()),
            Self::WorkFileParse(value) => Self::WorkFileParse(value.clone()),
            Self::ManifestParse(value) => Self::ManifestParse(value.clone()),
            Self::ExtManifestParse(value) => Self::ExtManifestParse(value.clone()),
            Self::SourceScan(value) => Self::SourceScan(value.clone()),
            Self::RegistryError(value) => Self::RegistryError(value.clone()),
            Self::RegistryNotFound { resource } => Self::RegistryNotFound {
                resource: resource.clone(),
            },
            Self::RegistryResponseTooLarge { resource, limit } => Self::RegistryResponseTooLarge {
                resource: resource.clone(),
                limit: *limit,
            },
            Self::InvalidReleaseMetadata(value) => Self::InvalidReleaseMetadata(value.clone()),
            Self::NoSatisfyingVersion { module, detail } => Self::NoSatisfyingVersion {
                module: module.clone(),
                detail: detail.clone(),
            },
            Self::ConflictingConstraints { module, detail } => Self::ConflictingConstraints {
                module: module.clone(),
                detail: detail.clone(),
            },
            Self::SelectedVersionConflict {
                module,
                existing,
                requested,
            } => Self::SelectedVersionConflict {
                module: module.clone(),
                existing: existing.clone(),
                requested: requested.clone(),
            },
            Self::DependencyToolchainMismatch {
                module,
                project_constraint,
                dependency_constraint,
            } => Self::DependencyToolchainMismatch {
                module: module.clone(),
                project_constraint: project_constraint.clone(),
                dependency_constraint: dependency_constraint.clone(),
            },
            Self::ResolutionLimitExceeded { resource, limit } => Self::ResolutionLimitExceeded {
                resource: resource.clone(),
                limit: *limit,
            },
            Self::RootMismatch {
                field,
                mod_value,
                lock_value,
            } => Self::RootMismatch {
                field: field.clone(),
                mod_value: mod_value.clone(),
                lock_value: lock_value.clone(),
            },
            Self::LockedModuleMismatch {
                module,
                field,
                expected,
                found,
            } => Self::LockedModuleMismatch {
                module: module.clone(),
                field: field.clone(),
                expected: expected.clone(),
                found: found.clone(),
            },
            Self::DigestMismatch {
                context,
                expected,
                found,
            } => Self::DigestMismatch {
                context: context.clone(),
                expected: expected.clone(),
                found: found.clone(),
            },
            Self::MissingArtifact {
                module,
                version,
                detail,
            } => Self::MissingArtifact {
                module: module.clone(),
                version: version.clone(),
                detail: detail.clone(),
            },
            Self::MissingLockedArtifact {
                module,
                version,
                detail,
            } => Self::MissingLockedArtifact {
                module: module.clone(),
                version: version.clone(),
                detail: detail.clone(),
            },
            Self::CachePublicationDurabilityUnconfirmed { path, message } => {
                Self::CachePublicationDurabilityUnconfirmed {
                    path: path.clone(),
                    message: message.clone(),
                }
            }
            Self::CachePublicationLocationUnconfirmed { path, message } => {
                Self::CachePublicationLocationUnconfirmed {
                    path: path.clone(),
                    message: message.clone(),
                }
            }
            Self::ProjectPublicationPostCommitFailure { path, message } => {
                Self::ProjectPublicationPostCommitFailure {
                    path: path.clone(),
                    message: message.clone(),
                }
            }
            Self::WorkspaceIdentityMismatch {
                expected,
                found,
                path,
            } => Self::WorkspaceIdentityMismatch {
                expected: expected.clone(),
                found: found.clone(),
                path: path.clone(),
            },
            Self::OverrideUnlockedDep {
                importer,
                import_path,
            } => Self::OverrideUnlockedDep {
                importer: importer.clone(),
                import_path: import_path.clone(),
            },
            Self::SelfOverride => Self::SelfOverride,
            Self::Io(error) => Self::Io(std::io::Error::new(error.kind(), error.to_string())),
            Self::Network(value) => Self::Network(value.clone()),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidModulePath(msg) => write!(f, "invalid module path: {msg}"),
            Self::InvalidVersion(msg) => write!(f, "invalid version: {msg}"),
            Self::InvalidConstraint(msg) => write!(f, "invalid constraint: {msg}"),
            Self::InvalidDigest(msg) => write!(f, "invalid digest: {msg}"),
            Self::InvalidImportPath(msg) => write!(f, "invalid import path: {msg}"),
            Self::DependencyGraph(msg) => write!(f, "module graph error: {msg}"),
            Self::ModFileParse(msg) => write!(f, "vo.mod parse error: {msg}"),
            Self::LockFileParse(msg) => write!(f, "vo.lock parse error: {msg}"),
            Self::WorkFileParse(msg) => write!(f, "vo.work parse error: {msg}"),
            Self::ManifestParse(msg) => write!(f, "vo.release.json parse error: {msg}"),
            Self::ExtManifestParse(msg) => write!(f, "vo.mod metadata parse error: {msg}"),
            Self::SourceScan(msg) => write!(f, "source scan error: {msg}"),
            Self::RegistryError(msg) => write!(f, "registry error: {msg}"),
            Self::RegistryNotFound { resource } => {
                write!(f, "registry resource not found: {resource}")
            }
            Self::RegistryResponseTooLarge { resource, limit } => write!(
                f,
                "registry response for {resource} exceeds the {limit}-byte limit"
            ),
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
            Self::SelectedVersionConflict {
                module,
                existing,
                requested,
            } => {
                write!(
                    f,
                    "module {module} was selected at both {existing} and {requested}"
                )
            }
            Self::DependencyToolchainMismatch {
                module,
                project_constraint,
                dependency_constraint,
            } => {
                write!(
                    f,
                    "dependency {module} requires toolchain {dependency_constraint} which is incompatible with project constraint {project_constraint}"
                )
            }
            Self::ResolutionLimitExceeded { resource, limit } => {
                write!(
                    f,
                    "dependency resolution {resource} exceeds the limit of {limit}"
                )
            }
            Self::RootMismatch {
                field,
                mod_value,
                lock_value,
            } => {
                write!(
                    f,
                    "root vo.mod vs vo.lock mismatch on {field}: vo.mod has {mod_value}, vo.lock has {lock_value}"
                )
            }
            Self::LockedModuleMismatch {
                module,
                field,
                expected,
                found,
            } => {
                write!(
                    f,
                    "vo.lock entry for {module} does not match published release manifest: {field}: expected {expected}, found {found}"
                )
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
            Self::MissingArtifact {
                module,
                version,
                detail,
            } => {
                write!(
                    f,
                    "required locked artifact is missing from cache: {module} {version}: {detail}\n  run: vo mod download"
                )
            }
            Self::MissingLockedArtifact {
                module,
                version,
                detail,
            } => {
                write!(
                    f,
                    "vo.lock is missing required artifact metadata: {module} {version}: {detail}"
                )
            }
            Self::CachePublicationDurabilityUnconfirmed { path, message } => {
                write!(
                    f,
                    "module cache publication to {path} committed, but durability confirmation failed: {message}"
                )
            }
            Self::CachePublicationLocationUnconfirmed { path, message } => {
                write!(
                    f,
                    "module cache publication to {path} committed on the held cache-root capability, but configured-path location confirmation failed: {message}"
                )
            }
            Self::ProjectPublicationPostCommitFailure { path, message } => {
                write!(
                    f,
                    "project file mutation at {path} committed, but a post-commit step failed: {message}"
                )
            }
            Self::WorkspaceIdentityMismatch {
                expected,
                found,
                path,
            } => {
                write!(
                    f,
                    "workspace override identity mismatch at {path}: expected module {expected}, found {found}"
                )
            }
            Self::OverrideUnlockedDep {
                importer,
                import_path,
            } => {
                write!(
                    f,
                    "local override imports an external module not in root lockfile: {importer} imports {import_path}\n  run: vo mod sync or remove the local replace/override"
                )
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
