use std::collections::BTreeSet;
use std::fmt;
use std::path::PathBuf;

use vo_module::ext_manifest::DeclaredArtifactId;

pub type ReleaseResult<T> = Result<T, ReleaseError>;

#[derive(Debug, Clone)]
pub enum ReleaseError {
    RepoRootNotDirectory(PathBuf),
    IoError(PathBuf, String),
    AtomicPublishUnsupported {
        path: PathBuf,
        message: String,
    },
    PublishedButDurabilityUnconfirmed {
        path: PathBuf,
        message: String,
    },
    ManifestSerialize(String),
    GitError {
        repo_root: PathBuf,
        message: String,
    },
    InvalidArtifactPath(PathBuf),
    ForbiddenVoSum {
        repo_root: PathBuf,
        paths: Vec<PathBuf>,
    },
    InvalidAliasImports(Vec<String>),
    ArtifactContractViolation {
        manifest_path: Option<PathBuf>,
        missing: Vec<DeclaredArtifactId>,
        undeclared: Vec<DeclaredArtifactId>,
    },
    Module(String),
}

impl From<vo_module::Error> for ReleaseError {
    fn from(e: vo_module::Error) -> Self {
        ReleaseError::Module(e.to_string())
    }
}

const MAX_DIAGNOSTIC_ITEMS: usize = 8;

pub(crate) fn bounded_sorted_diagnostic<I, S>(items: I) -> String
where
    I: IntoIterator<Item = S>,
    S: Into<String>,
{
    let mut first = BTreeSet::new();
    let mut total = 0usize;
    for item in items {
        total = total.saturating_add(1);
        first.insert(item.into());
        if first.len() > MAX_DIAGNOSTIC_ITEMS {
            first.pop_last();
        }
    }
    let shown = first.into_iter().collect::<Vec<_>>().join(", ");
    let remaining = total.saturating_sub(MAX_DIAGNOSTIC_ITEMS.min(total));
    if remaining == 0 {
        shown
    } else if shown.is_empty() {
        format!("and {remaining} more")
    } else {
        format!("{shown} (and {remaining} more)")
    }
}

fn format_declared_artifacts(artifacts: &[DeclaredArtifactId]) -> String {
    bounded_sorted_diagnostic(
        artifacts
            .iter()
            .map(|artifact| format!("{}:{}:{}", artifact.kind, artifact.target, artifact.name)),
    )
}

impl fmt::Display for ReleaseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReleaseError::RepoRootNotDirectory(path) => {
                write!(
                    f,
                    "repo root does not exist or is not a directory: {}",
                    path.display()
                )
            }
            ReleaseError::IoError(path, message) => {
                write!(f, "failed to access {}: {}", path.display(), message)
            }
            ReleaseError::AtomicPublishUnsupported { path, message } => write!(
                f,
                "the filesystem cannot provide atomic durable release publication at {}: {}",
                path.display(),
                message,
            ),
            ReleaseError::PublishedButDurabilityUnconfirmed { path, message } => write!(
                f,
                "release output was published through the anchored parent of {}, but the final path binding or durable persistence could not be confirmed: {}",
                path.display(),
                message,
            ),
            ReleaseError::ManifestSerialize(message) => {
                write!(f, "failed to render vo.release.json: {}", message)
            }
            ReleaseError::GitError { repo_root, message } => {
                write!(
                    f,
                    "failed to read git commit for {}: {}",
                    repo_root.display(),
                    message
                )
            }
            ReleaseError::InvalidArtifactPath(path) => {
                write!(f, "artifact file does not exist: {}", path.display())
            }
            ReleaseError::ForbiddenVoSum { repo_root, paths } => {
                let formatted = bounded_sorted_diagnostic(
                    paths.iter().map(|path| path.display().to_string()),
                );
                write!(
                    f,
                    "forbidden vo.sum files present in {}: {}",
                    repo_root.display(),
                    formatted
                )
            }
            ReleaseError::InvalidAliasImports(violations) => {
                write!(
                    f,
                    "old alias import syntax remains: {}",
                    bounded_sorted_diagnostic(violations.iter().cloned())
                )
            }
            ReleaseError::ArtifactContractViolation {
                manifest_path,
                missing,
                undeclared,
            } => {
                match manifest_path {
                    Some(path) => write!(
                        f,
                        "staged artifacts do not match the declared release artifact contract in {}",
                        path.display()
                    )?,
                    None => write!(
                        f,
                        "staged artifacts were provided, but vo.mod does not declare any release artifacts"
                    )?,
                }
                if !missing.is_empty() {
                    write!(f, "\nmissing: {}", format_declared_artifacts(missing))?;
                }
                if !undeclared.is_empty() {
                    write!(f, "\nundeclared: {}", format_declared_artifacts(undeclared))?;
                }
                Ok(())
            }
            ReleaseError::Module(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for ReleaseError {}
