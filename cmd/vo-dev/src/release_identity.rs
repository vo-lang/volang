use anyhow::{anyhow, bail, Context, Result};
use semver::Version;
use serde::{Deserialize, Serialize};
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ReleaseIdentity {
    pub(crate) tag: String,
    pub(crate) version: String,
    pub(crate) commit: String,
    pub(crate) build_date: String,
    pub(crate) source_date_epoch: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum SourceCleanliness {
    AllFiles,
    TrackedFiles,
}

pub(crate) fn workspace_version(root: &Path) -> Result<String> {
    let path = root.join("Cargo.toml");
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let manifest = text
        .parse::<toml::Table>()
        .with_context(|| format!("could not parse {}", path.display()))?;
    let version = manifest
        .get("workspace")
        .and_then(toml::Value::as_table)
        .and_then(|workspace| workspace.get("package"))
        .and_then(toml::Value::as_table)
        .and_then(|package| package.get("version"))
        .and_then(toml::Value::as_str)
        .ok_or_else(|| anyhow!("Cargo.toml has no workspace.package.version"))?;
    validate_canonical_semver("workspace package version", version)?;
    Ok(version.to_string())
}

pub(crate) fn validated_release_version(root: &Path, tag: &str) -> Result<String> {
    let version = workspace_version(root)?;
    let expected = format!("v{version}");
    if tag != expected {
        bail!("release tag must exactly match workspace version: expected {expected}, got {tag}");
    }
    validate_canonical_semver("release tag version", &tag[1..])?;
    Ok(version)
}

pub(crate) fn resolve_release_identity(
    root: &Path,
    explicit_tag: Option<&str>,
    explicit_commit: Option<&str>,
    cleanliness: SourceCleanliness,
) -> Result<ReleaseIdentity> {
    let tag = consistent_value(
        "release tag",
        explicit_tag,
        &[
            ("VO_RELEASE_TAG", env::var("VO_RELEASE_TAG").ok()),
            ("GITHUB_REF_NAME", env::var("GITHUB_REF_NAME").ok()),
        ],
    )?
    .ok_or_else(|| {
        anyhow!("release identity requires --tag, VO_RELEASE_TAG, or GITHUB_REF_NAME")
    })?;
    let version = validated_release_version(root, &tag)?;

    let head = git_output(root, &["rev-parse", "--verify", "HEAD^{commit}"])?;
    validate_commit("current HEAD", &head)?;
    let tag_ref = format!("refs/tags/{tag}^{{commit}}");
    let tag_commit = git_output(root, &["rev-parse", "--verify", &tag_ref])?;
    validate_commit("release tag commit", &tag_commit)?;
    if head != tag_commit {
        bail!("release tag {tag} resolves to {tag_commit}, while the checkout is {head}");
    }

    let requested_commit = consistent_value(
        "release commit",
        explicit_commit,
        &[
            ("VO_BUILD_COMMIT", env::var("VO_BUILD_COMMIT").ok()),
            ("GITHUB_SHA", env::var("GITHUB_SHA").ok()),
        ],
    )?;
    if let Some(requested_commit) = requested_commit {
        validate_commit("release commit", &requested_commit)?;
        if requested_commit.to_ascii_lowercase() != head {
            bail!(
                "release commit must equal tagged checkout commit {head}, got {requested_commit}"
            );
        }
    }

    ensure_clean_checkout(root, cleanliness)?;

    let date_and_epoch = git_output(root, &["show", "-s", "--format=%cI%n%ct", &head])?;
    let mut lines = date_and_epoch.lines();
    let build_date = lines
        .next()
        .filter(|line| !line.is_empty())
        .ok_or_else(|| anyhow!("git returned no commit date for {head}"))?
        .to_string();
    let epoch_text = lines
        .next()
        .filter(|line| !line.is_empty())
        .ok_or_else(|| anyhow!("git returned no commit epoch for {head}"))?;
    if lines.next().is_some() {
        bail!("git returned unexpected commit metadata for {head}");
    }
    let source_date_epoch = epoch_text
        .parse::<u64>()
        .with_context(|| format!("git returned invalid commit epoch {epoch_text:?}"))?;
    if source_date_epoch > u32::MAX as u64 {
        bail!("commit epoch {source_date_epoch} exceeds deterministic gzip timestamp range");
    }

    validate_optional_env("VO_BUILD_DATE", &build_date)?;
    validate_optional_env("SOURCE_DATE_EPOCH", epoch_text)?;

    Ok(ReleaseIdentity {
        tag,
        version,
        commit: head,
        build_date,
        source_date_epoch,
    })
}

fn consistent_value(
    field: &str,
    explicit: Option<&str>,
    environment: &[(&str, Option<String>)],
) -> Result<Option<String>> {
    let mut selected = explicit.map(str::to_string);
    for (name, candidate) in environment {
        let Some(candidate) = candidate else {
            continue;
        };
        if candidate.is_empty() || candidate.trim() != candidate {
            bail!("{name} must be non-empty and have no surrounding whitespace");
        }
        if let Some(selected) = &selected {
            if selected != candidate {
                bail!("conflicting {field}: {selected} and {name}={candidate}");
            }
        } else {
            selected = Some(candidate.clone());
        }
    }
    Ok(selected)
}

fn validate_optional_env(name: &str, expected: &str) -> Result<()> {
    let Ok(actual) = env::var(name) else {
        return Ok(());
    };
    if actual != expected {
        bail!("{name} must match tagged commit metadata: expected {expected}, got {actual}");
    }
    Ok(())
}

fn ensure_clean_checkout(root: &Path, cleanliness: SourceCleanliness) -> Result<()> {
    let untracked = match cleanliness {
        SourceCleanliness::AllFiles => "--untracked-files=normal",
        SourceCleanliness::TrackedFiles => "--untracked-files=no",
    };
    let status = git_output(root, &["status", "--porcelain=v1", untracked])?;
    if !status.is_empty() {
        let first = status.lines().take(8).collect::<Vec<_>>().join("; ");
        bail!("release checkout contains source changes: {first}");
    }
    Ok(())
}

fn git_output(root: &Path, args: &[&str]) -> Result<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(root)
        .output()
        .with_context(|| format!("could not run git {}", args.join(" ")))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        bail!("git {} failed: {stderr}", args.join(" "));
    }
    let stdout = String::from_utf8(output.stdout).context("git output was not UTF-8")?;
    Ok(stdout.trim_end_matches(['\r', '\n']).to_string())
}

fn validate_commit(field: &str, value: &str) -> Result<()> {
    if !matches!(value.len(), 40 | 64) || !value.chars().all(|ch| ch.is_ascii_hexdigit()) {
        bail!("{field} must be a full 40- or 64-character hexadecimal commit id");
    }
    if value != value.to_ascii_lowercase() {
        bail!("{field} must use lowercase hexadecimal");
    }
    Ok(())
}

fn validate_canonical_semver(field: &str, value: &str) -> Result<()> {
    let parsed = Version::parse(value).with_context(|| format!("{field} is not valid SemVer"))?;
    if parsed.to_string() != value {
        bail!("{field} must use canonical SemVer spelling: {value}");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_semver_rejects_leading_zeroes() {
        assert!(validate_canonical_semver("version", "0.01.0").is_err());
    }

    #[test]
    fn commit_validation_requires_full_lowercase_hex() {
        assert!(validate_commit("commit", &"a".repeat(40)).is_ok());
        assert!(validate_commit("commit", &"A".repeat(40)).is_err());
        assert!(validate_commit("commit", "deadbeef").is_err());
    }

    #[test]
    fn consistent_values_reject_drift() {
        let environment = [("VO_RELEASE_TAG", Some("v0.2.0".to_string()))];
        let error = consistent_value("release tag", Some("v0.1.0"), &environment).unwrap_err();
        assert!(error.to_string().contains("conflicting release tag"));
    }

    #[test]
    fn release_tag_must_exactly_match_workspace_version() {
        let root = env::temp_dir().join(format!("vo-dev-release-version-{}", std::process::id()));
        if root.exists() {
            fs::remove_dir_all(&root).unwrap();
        }
        fs::create_dir_all(&root).unwrap();
        fs::write(
            root.join("Cargo.toml"),
            "[workspace]\n[workspace.package]\nversion = \"1.2.3-rc.1\"\n",
        )
        .unwrap();
        assert_eq!(
            validated_release_version(&root, "v1.2.3-rc.1").unwrap(),
            "1.2.3-rc.1"
        );
        assert!(validated_release_version(&root, "1.2.3-rc.1").is_err());
        assert!(validated_release_version(&root, "v1.2.3").is_err());
        fs::remove_dir_all(root).unwrap();
    }
}
