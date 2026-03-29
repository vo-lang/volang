mod error;
mod repo;

pub use error::{ReleaseError, ReleaseResult};
pub use repo::{
    stage_release, verify_repo, ArtifactInput, StageReleaseOptions, StagedArtifact, StagedRelease,
};

#[cfg(test)]
mod tests;
