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
            if matches!(scheme, "external" | "module-cache" | "first-party") {
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
    let Some((scheme, reference)) = input.split_once(':') else {
        return Ok(());
    };
    let (repo, repo_path) = reference
        .split_once('/')
        .map_or((reference, None), |(repo, path)| (repo, Some(path)));
    if repo.trim().is_empty() || repo.trim() != repo {
        bail!("{owner_kind} {owner_name} input {scheme}: reference has an invalid repository name");
    }
    if let Some(repo_path) = repo_path {
        validate_repo_path_like(
            owner_kind,
            owner_name,
            "structured input path",
            repo_path,
            false,
        )?;
    }
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
        "first-party" => {
            if !project.first_party.iter().any(|item| item.name == repo) {
                bail!(
                    "{owner_kind} {owner_name} input first-party:{repo} must reference a first-party repo"
                );
            }
        }
        _ => {}
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::validate_structured_input_reference;
    use crate::config::{ProjectFile, ProjectRepo, Repo};

    fn project() -> ProjectFile {
        ProjectFile {
            version: 1,
            repo: Repo {
                name: "volang".into(),
                module: "github.com/vo-lang/volang".into(),
            },
            first_party: vec![ProjectRepo {
                name: "voplay".into(),
                repository: None,
                local_hint: None,
                expected_commit: None,
                ci_checkout: None,
                workspace: Vec::new(),
            }],
            external_project: vec![ProjectRepo {
                name: "BlockKart".into(),
                repository: None,
                local_hint: None,
                expected_commit: None,
                ci_checkout: None,
                workspace: Vec::new(),
            }],
        }
    }

    #[test]
    fn structured_input_accepts_declared_repo_subpaths() {
        let project = project();
        validate_structured_input_reference(
            "artifact",
            "quickplay",
            "external:BlockKart/tools/pack_primitive_assets.vo",
            &project,
        )
        .unwrap();
        validate_structured_input_reference(
            "task",
            "render",
            "first-party:voplay/rust/src/renderer.rs",
            &project,
        )
        .unwrap();
    }

    #[test]
    fn structured_input_rejects_undeclared_repo_and_invalid_subpath() {
        let project = project();
        assert!(validate_structured_input_reference(
            "artifact",
            "quickplay",
            "external:Unknown/tools/pack.vo",
            &project,
        )
        .is_err());
        assert!(validate_structured_input_reference(
            "artifact",
            "quickplay",
            "external:BlockKart/tools/../pack.vo",
            &project,
        )
        .is_err());
    }
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
