use crate::config::ProjectFile;
use anyhow::{bail, Result};
use std::collections::HashSet;
use std::path::Path;

pub(crate) fn validate_repo_path_like(
    owner_kind: &str,
    owner_name: &str,
    field: &str,
    value: &str,
    allow_artifact_input_scheme: bool,
) -> Result<()> {
    if allow_artifact_input_scheme {
        if let Some((scheme, rest)) = value.split_once(':') {
            if matches!(scheme, "external" | "module-cache") {
                if rest.trim().is_empty() || rest.trim() != rest {
                    bail!("{owner_kind} {owner_name} {field} has invalid {scheme}: reference");
                }
                return Ok(());
            }
        }
    }
    if value.trim().is_empty() {
        bail!("{owner_kind} {owner_name} {field} cannot be empty");
    }
    if value.trim() != value {
        bail!("{owner_kind} {owner_name} {field} cannot contain surrounding whitespace");
    }
    if Path::new(value).is_absolute() {
        bail!("{owner_kind} {owner_name} {field} must be repo-relative");
    }
    if value.contains(':') {
        bail!("{owner_kind} {owner_name} {field} cannot contain ':'");
    }
    if value.contains('\\') {
        bail!("{owner_kind} {owner_name} {field} must use / separators");
    }
    if value.contains("//") {
        bail!("{owner_kind} {owner_name} {field} cannot contain empty path segments");
    }
    let trimmed = value.trim_end_matches('/');
    if trimmed.is_empty() {
        bail!("{owner_kind} {owner_name} {field} cannot point to repository root");
    }
    for segment in trimmed.split('/') {
        if segment.is_empty() || segment == "." || segment == ".." {
            bail!("{owner_kind} {owner_name} {field} contains invalid segment {segment:?}");
        }
    }
    Ok(())
}

pub(crate) fn validate_structured_input_reference(
    owner_kind: &str,
    owner_name: &str,
    input: &str,
    project: &ProjectFile,
) -> Result<()> {
    let Some((scheme, repo)) = input.split_once(':') else {
        return Ok(());
    };
    match scheme {
        "external" => {
            if !project
                .external_project
                .iter()
                .any(|item| item.name == repo)
            {
                bail!(
                    "{owner_kind} {owner_name} input external:{repo} is not declared in eng/project.toml"
                );
            }
        }
        "module-cache" => {
            if !project.first_party.iter().any(|item| item.name == repo) {
                bail!(
                    "{owner_kind} {owner_name} input module-cache:{repo} must reference a first-party repo"
                );
            }
        }
        _ => {}
    }
    Ok(())
}

pub(crate) fn contains_glob_meta(value: &str) -> bool {
    value.contains('*') || value.contains('?') || value.contains('[') || value.contains(']')
}

pub(crate) fn artifact_path_contains(parent: &str, child: &str) -> bool {
    child == parent || child.starts_with(&format!("{}/", parent.trim_end_matches('/')))
}

pub(crate) fn validate_ascii_slug(kind: &str, value: &str, extra: &[char]) -> Result<()> {
    if value.trim().is_empty() {
        bail!("{kind} cannot be empty");
    }
    if value.trim() != value {
        bail!("{kind} {value:?} cannot contain surrounding whitespace");
    }
    if !value
        .chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || extra.contains(&ch))
    {
        bail!("{kind} {value} must use lowercase ASCII letters, digits, or approved punctuation");
    }
    Ok(())
}

pub(crate) fn validate_unique_values(
    owner_kind: &str,
    owner_name: &str,
    field: &str,
    values: &[String],
) -> Result<()> {
    let mut seen = HashSet::new();
    for value in values {
        if !seen.insert(value) {
            bail!("{owner_kind} {owner_name} has duplicate {field} {value}");
        }
    }
    Ok(())
}

pub(crate) fn declared_repo_names(project: &ProjectFile) -> HashSet<String> {
    project
        .first_party
        .iter()
        .chain(project.external_project.iter())
        .map(|repo| repo.name.clone())
        .collect()
}
