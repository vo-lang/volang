use std::fmt;
use std::path::PathBuf;

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
    LegacyAliasImports(Vec<String>),
    Module(String),
}

impl From<vo_module::Error> for ReleaseError {
    fn from(e: vo_module::Error) -> Self {
        ReleaseError::Module(e.to_string())
    }
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
            ReleaseError::LegacyAliasImports(violations) => {
                write!(f, "old alias import syntax remains")?;
                for violation in violations {
                    write!(f, "\n{}", violation)?;
                }
                Ok(())
            }
            ReleaseError::Module(msg) => write!(f, "{}", msg),
        }
    }
}
