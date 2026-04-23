use std::fmt;
use std::path::PathBuf;

use vo_module::ext_manifest::DeclaredArtifactId;

pub type ReleaseResult<T> = Result<T, ReleaseError>;

#[derive(Debug, Clone)]
pub enum ReleaseError {
    RepoRootNotDirectory(PathBuf),
    IoError(PathBuf, String),
    ManifestSerialize(String),
    GitError {
        repo_root: PathBuf,
        message: String,
    },
    InvalidArtifactPath(PathBuf),
    DuplicateArtifactName(String),
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

fn format_declared_artifacts(artifacts: &[DeclaredArtifactId]) -> String {
    artifacts
        .iter()
        .map(|artifact| format!("{}:{}:{}", artifact.kind, artifact.target, artifact.name))
        .collect::<Vec<_>>()
        .join(", ")
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
            ReleaseError::DuplicateArtifactName(name) => {
                write!(f, "duplicate staged artifact name: {}", name)
            }
            ReleaseError::ForbiddenVoSum { repo_root, paths } => {
                let formatted = paths
                    .iter()
                    .map(|path| path.display().to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "forbidden vo.sum files present in {}: {}",
                    repo_root.display(),
                    formatted
                )
            }
            ReleaseError::InvalidAliasImports(violations) => {
                write!(f, "old alias import syntax remains")?;
                for violation in violations {
                    write!(f, "\n{}", violation)?;
                }
                Ok(())
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
