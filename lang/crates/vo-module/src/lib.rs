pub mod artifact;
pub mod async_install;
mod async_solver;
pub mod cache;
pub mod digest;
pub mod ext_manifest;
pub mod github_provenance;
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
pub mod snapshot;
pub mod solver;
pub mod version;
#[cfg(windows)]
mod windows_file;
pub mod workspace;

use std::fmt;

/// Canonical version of the Volang toolchain that owns this module protocol.
pub const TOOLCHAIN_VERSION: &str = env!("CARGO_PKG_VERSION");
/// Compatible toolchain constraint written into newly initialized modules.
pub const TOOLCHAIN_CONSTRAINT: &str = concat!("^", env!("CARGO_PKG_VERSION"));

/// Hard ceiling for a single target artifact materialized by the module system.
pub const MAX_MODULE_ARTIFACT_BYTES: u64 = 256 * 1024 * 1024;
/// `usize` form for in-process readers on every supported target.
pub const MAX_MODULE_ARTIFACT_BYTES_USIZE: usize = MAX_MODULE_ARTIFACT_BYTES as usize;
/// Hard ceiling for a compressed module source archive. Together with the
/// extracted limits below, this keeps package decode below a predictable
/// memory envelope on native and browser installers.
pub const MAX_SOURCE_ARCHIVE_BYTES: u64 = 64 * 1024 * 1024;
/// Hard ceiling for entries in one module source archive.
pub const MAX_SOURCE_ARCHIVE_ENTRIES: usize = 100_000;
/// Hard ceiling for one extracted source-archive entry.
pub const MAX_SOURCE_ARCHIVE_ENTRY_BYTES: usize = 64 * 1024 * 1024;
/// Hard ceiling for total extracted bytes in one source archive, including
/// the embedded package manifest.
pub const MAX_EXTRACTED_SOURCE_BYTES: usize = 128 * 1024 * 1024;
/// Hard ceiling for aggregate UTF-8 bytes retained by a canonical package
/// path trie. Each new node is charged by its complete relative path,
/// including implicit directory prefixes.
pub const MAX_PACKAGE_PATH_KEY_BYTES: usize = 16 * 1024 * 1024;
/// Hard ceiling for direct dependencies declared by one module or manifest.
pub const MAX_MODULE_DEPENDENCIES: usize = 10_000;
/// Hard ceiling for target artifacts declared by one module version. GitHub
/// Releases allows 1,000 assets; the fixed release, package, and source assets
/// reserve three entries from that inventory.
pub const MAX_MODULE_ARTIFACTS: usize = 997;
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

const MAX_RENDERED_DIAGNOSTIC_ITEMS: usize = 8;

/// Render the lexicographically smallest diagnostic items with bounded memory
/// and an explicit omitted count.
pub(crate) fn summarize_diagnostic_items(
    items: impl IntoIterator<Item = String>,
    omitted_label: &str,
) -> String {
    let mut shown = Vec::<String>::with_capacity(MAX_RENDERED_DIAGNOSTIC_ITEMS);
    let mut total = 0usize;
    for item in items {
        total = total.saturating_add(1);
        let position = shown.partition_point(|existing| existing <= &item);
        if shown.len() < MAX_RENDERED_DIAGNOSTIC_ITEMS {
            shown.insert(position, item);
        } else if position < MAX_RENDERED_DIAGNOSTIC_ITEMS {
            shown.insert(position, item);
            shown.pop();
        }
    }
    let mut detail = shown.join(", ");
    let omitted = total.saturating_sub(shown.len());
    if omitted > 0 {
        detail.push_str(&format!(", ... and {omitted} more {omitted_label}"));
    }
    detail
}

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
    SourceRead(std::io::Error),

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
    MissingReleaseArtifact {
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
    WorkspaceValidation(String),
    WorkspaceSourceOutsideGraph {
        importer: String,
        import_path: String,
    },

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
            Self::SourceRead(error) => {
                Self::SourceRead(std::io::Error::new(error.kind(), error.to_string()))
            }
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
            Self::MissingReleaseArtifact {
                module,
                version,
                detail,
            } => Self::MissingReleaseArtifact {
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
            Self::WorkspaceValidation(value) => Self::WorkspaceValidation(value.clone()),
            Self::WorkspaceSourceOutsideGraph {
                importer,
                import_path,
            } => Self::WorkspaceSourceOutsideGraph {
                importer: importer.clone(),
                import_path: import_path.clone(),
            },
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
            Self::SourceRead(error) => write!(f, "source read error: {error}"),
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
                    "required locked artifact is missing from cache: {module} {version}: {detail}\n  run: vo mod fetch"
                )
            }
            Self::MissingReleaseArtifact {
                module,
                version,
                detail,
            } => {
                write!(
                    f,
                    "vo.release.json does not declare the required artifact: {module} {version}: {detail}"
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
            Self::WorkspaceValidation(msg) => write!(f, "workspace validation error: {msg}"),
            Self::WorkspaceSourceOutsideGraph {
                importer,
                import_path,
            } => {
                write!(
                    f,
                    "source import is outside the authoritative dependency graph: {importer} imports {import_path}\n  declare the dependency in the importing module and select a complete workspace, or run: vo mod sync"
                )
            }
            Self::Io(e) => write!(f, "io error: {e}"),
            Self::Network(msg) => write!(f, "network error: {msg}"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(error) | Self::SourceRead(error) => Some(error),
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

#[cfg(test)]
mod diagnostic_summary_tests {
    #[test]
    fn source_package_payload_memory_envelope_is_bounded() {
        const MAX_RESIDENT_PAYLOAD_BYTES: usize = 192 * 1024 * 1024;
        let compressed = usize::try_from(super::MAX_SOURCE_ARCHIVE_BYTES).unwrap();
        assert!(
            compressed + super::MAX_EXTRACTED_SOURCE_BYTES <= MAX_RESIDENT_PAYLOAD_BYTES,
            "decode retains the compressed archive and one extracted source tree",
        );
        const {
            assert!(
                super::MAX_EXTRACTED_SOURCE_BYTES + super::MAX_SOURCE_ARCHIVE_ENTRY_BYTES
                    <= MAX_RESIDENT_PAYLOAD_BYTES,
                "staging retains one tree plus at most one verification read",
            );
        }
    }

    #[test]
    fn diagnostic_summaries_are_deterministic_and_bounded() {
        let values = (0..100_000)
            .rev()
            .map(|index| format!("item-{index:06}"))
            .collect::<Vec<_>>();
        let forward = super::summarize_diagnostic_items(values.iter().cloned(), "item(s)");
        let reverse = super::summarize_diagnostic_items(values.iter().rev().cloned(), "item(s)");
        assert_eq!(forward, reverse);
        assert!(forward.len() < 1024);
        assert!(forward.contains("item-000000"));
        assert!(forward.contains("item-000007"));
        assert!(!forward.contains("item-000008"));
        assert!(forward.contains("99992 more item(s)"));
    }
}
