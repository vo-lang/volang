use anyhow::{Context, Result};
use serde::Deserialize;
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

#[derive(Debug, Deserialize)]
pub(crate) struct ToolchainFile {
    pub(crate) version: u32,
    #[serde(default)]
    pub(crate) tools: BTreeMap<String, Tool>,
    #[serde(default)]
    pub(crate) node_workspace: Vec<NodeWorkspace>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct Tool {
    pub(crate) version: Option<String>,
    pub(crate) version_from: Option<String>,
    pub(crate) source: Option<String>,
    pub(crate) minimum: Option<String>,
    pub(crate) required: Option<bool>,
    pub(crate) usage: Option<String>,
    pub(crate) check: Option<Vec<String>>,
    pub(crate) bootstrap: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct NodeWorkspace {
    pub(crate) name: String,
    pub(crate) repo: Option<String>,
    pub(crate) path: String,
    pub(crate) lockfile: String,
    pub(crate) status: Option<String>,
    pub(crate) audit: Option<String>,
    pub(crate) audit_level: Option<String>,
    pub(crate) audit_reason: Option<String>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct ProjectFile {
    pub(crate) version: u32,
    pub(crate) repo: Repo,
    #[serde(default)]
    pub(crate) first_party: Vec<ProjectRepo>,
    #[serde(default)]
    pub(crate) external_project: Vec<ProjectRepo>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct Repo {
    pub(crate) name: String,
    pub(crate) module: String,
}

#[derive(Debug, Deserialize)]
pub(crate) struct ProjectRepo {
    pub(crate) name: String,
    pub(crate) repository: Option<String>,
    pub(crate) local_hint: Option<String>,
    pub(crate) expected_commit: Option<String>,
    #[serde(default)]
    pub(crate) workspace: Vec<ProjectWorkspace>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct ProjectWorkspace {
    pub(crate) name: String,
    pub(crate) kind: String,
    pub(crate) path: String,
}

#[derive(Debug, Deserialize)]
pub(crate) struct ArtifactFile {
    pub(crate) version: u32,
    #[serde(default, rename = "artifact")]
    pub(crate) artifacts: Vec<Artifact>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct Artifact {
    pub(crate) name: String,
    #[serde(rename = "class")]
    pub(crate) class_name: String,
    pub(crate) path: String,
    pub(crate) owner: Option<String>,
    pub(crate) generator: Option<Vec<String>>,
    pub(crate) provenance: Option<String>,
    pub(crate) max_total_bytes: Option<u64>,
    #[serde(default)]
    pub(crate) allowed_extensions: Vec<String>,
    #[serde(default)]
    pub(crate) inputs: Vec<String>,
    pub(crate) tracked: Option<bool>,
    pub(crate) approval: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ReleaseFile {
    pub(crate) version: u32,
    pub(crate) package: ReleasePackage,
    pub(crate) sdk: ReleaseSdk,
    pub(crate) notes: ReleaseNotes,
    pub(crate) homebrew: ReleaseHomebrew,
    #[serde(default, rename = "target")]
    pub(crate) targets: Vec<ReleaseTarget>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ReleaseSdk {
    pub(crate) registry: String,
    #[serde(default)]
    pub(crate) internal_standalone: Vec<String>,
    #[serde(default)]
    pub(crate) packages: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ReleasePackage {
    #[serde(rename = "crate")]
    pub(crate) crate_name: String,
    pub(crate) binary: String,
    pub(crate) artifact_prefix: String,
    #[serde(default)]
    pub(crate) build_args: Vec<String>,
    pub(crate) release_opt_level: String,
    pub(crate) release_lto: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ReleaseNotes {
    pub(crate) product_name: String,
    #[serde(default)]
    pub(crate) homebrew: Vec<String>,
    pub(crate) manual_install: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ReleaseHomebrew {
    pub(crate) repository: String,
    pub(crate) formula_path: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ReleaseTarget {
    pub(crate) target: String,
    pub(crate) os: String,
}

pub(crate) fn load_toolchains(root: &Path) -> Result<ToolchainFile> {
    load_toml(&root.join("eng/toolchains.toml"))
}

pub(crate) fn load_project(root: &Path) -> Result<ProjectFile> {
    load_toml(&root.join("eng/project.toml"))
}

pub(crate) fn load_artifacts(root: &Path) -> Result<ArtifactFile> {
    load_toml(&root.join("eng/artifacts.toml"))
}

pub(crate) fn load_release(root: &Path) -> Result<ReleaseFile> {
    load_toml(&root.join("eng/release.toml"))
}

fn load_toml<T: for<'de> Deserialize<'de>>(path: &Path) -> Result<T> {
    let text =
        fs::read_to_string(path).with_context(|| format!("could not read {}", path.display()))?;
    toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))
}
