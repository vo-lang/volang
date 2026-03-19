mod error;
mod repo;

pub use error::{ReleaseError, ReleaseResult};
pub use repo::{
    ArtifactInput,
    StageReleaseOptions,
    StagedArtifact,
    StagedRelease,
    stage_release,
    verify_repo,
};

#[cfg(test)]
mod tests;
