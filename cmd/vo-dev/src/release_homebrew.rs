use crate::config::ReleaseFile;
use anyhow::{anyhow, bail, Result};
use semver::Version;
use std::collections::BTreeSet;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum HomebrewVersionProgression {
    Superseded,
    Current,
    Advance,
}

pub(crate) fn homebrew_checkout_path(release: &ReleaseFile) -> Result<String> {
    let repo = release
        .homebrew
        .repository
        .split_once('/')
        .map(|(_, repo)| repo)
        .ok_or_else(|| anyhow!("release homebrew repository must be owner/repo"))?;
    validate_homebrew_repo_token("release homebrew checkout path", repo)?;
    Ok(repo.to_string())
}

pub(crate) fn validate_homebrew_formula_targets(release: &ReleaseFile, text: &str) -> Result<()> {
    let expected = release
        .targets
        .iter()
        .map(|target| target.target.clone())
        .collect::<BTreeSet<_>>();
    let actual = homebrew_formula_targets(text)?;
    if actual != expected {
        let missing = expected
            .difference(&actual)
            .cloned()
            .collect::<Vec<_>>()
            .join(", ");
        let unexpected = actual
            .difference(&expected)
            .cloned()
            .collect::<Vec<_>>()
            .join(", ");
        bail!(
            "Homebrew formula target set differs from eng/release.toml; missing: {}; unexpected: {}",
            if missing.is_empty() { "none" } else { &missing },
            if unexpected.is_empty() {
                "none"
            } else {
                &unexpected
            }
        );
    }
    Ok(())
}

fn homebrew_formula_targets(text: &str) -> Result<BTreeSet<String>> {
    let mut targets = BTreeSet::new();
    let mut in_map = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if !in_map {
            if trimmed.starts_with("sha256_by_target") && trimmed.ends_with('{') {
                in_map = true;
            }
            continue;
        }
        if trimmed.starts_with('}') {
            return Ok(targets);
        }
        if let Some(target) = formula_target_line(trimmed) {
            targets.insert(target.to_string());
        }
    }
    bail!("Homebrew formula is missing sha256_by_target map")
}

fn formula_target_line(line: &str) -> Option<&str> {
    let rest = line.strip_prefix('"')?;
    let end = rest.find('"')?;
    let target = &rest[..end];
    let after_target = rest[end + 1..].trim_start();
    if after_target.starts_with("=>") {
        Some(target)
    } else {
        None
    }
}

pub(crate) fn replace_release_version(text: &str, version: &str) -> Result<String> {
    let mut count = 0;
    let lines = text
        .lines()
        .map(|line| {
            if let Some(prefix) = line.strip_suffix_after("release_version = \"") {
                if line.trim_start().starts_with("release_version = \"") {
                    count += 1;
                    return format!("{prefix}release_version = \"{version}\"");
                }
            }
            line.to_string()
        })
        .collect::<Vec<_>>();
    if count != 1 {
        bail!("failed to update Formula release_version");
    }
    Ok(preserve_trailing_newline(text, lines))
}

pub(crate) fn homebrew_version_progression(
    text: &str,
    target: &str,
) -> Result<HomebrewVersionProgression> {
    let mut versions = text.lines().filter_map(|line| {
        line.trim()
            .strip_prefix("release_version = \"")
            .and_then(|value| value.strip_suffix('"'))
    });
    let current = versions
        .next()
        .ok_or_else(|| anyhow!("Homebrew formula is missing release_version"))?;
    if versions.next().is_some() {
        bail!("Homebrew formula has duplicate release_version declarations");
    }
    let current_version = Version::parse(current).map_err(|error| {
        anyhow!("Homebrew formula release_version {current:?} is invalid: {error}")
    })?;
    let target_version = Version::parse(target).map_err(|error| {
        anyhow!("target Homebrew release version {target:?} is invalid: {error}")
    })?;
    Ok(match current_version.cmp(&target_version) {
        std::cmp::Ordering::Greater => HomebrewVersionProgression::Superseded,
        std::cmp::Ordering::Equal => HomebrewVersionProgression::Current,
        std::cmp::Ordering::Less => HomebrewVersionProgression::Advance,
    })
}

pub(crate) fn validate_release_version_progression(text: &str, target: &str) -> Result<()> {
    if homebrew_version_progression(text, target)? == HomebrewVersionProgression::Superseded {
        let current = text
            .lines()
            .find_map(|line| {
                line.trim()
                    .strip_prefix("release_version = \"")
                    .and_then(|value| value.strip_suffix('"'))
            })
            .ok_or_else(|| anyhow!("Homebrew formula is missing release_version"))?;
        bail!("refusing to downgrade Homebrew formula from {current} to {target}");
    }
    Ok(())
}

pub(crate) fn replace_formula_target_sha(text: &str, target: &str, sha: &str) -> Result<String> {
    let needle = format!("\"{target}\"");
    let mut count = 0;
    let lines = text
        .lines()
        .map(|line| {
            if !line.contains(&needle) || !line.contains("=>") {
                return line.to_string();
            }
            let Some(value_start) = line.find("=>") else {
                return line.to_string();
            };
            let before_value = &line[..value_start + 2];
            let after_arrow = &line[value_start + 2..];
            let Some(first_quote) = after_arrow.find('"') else {
                return line.to_string();
            };
            let rest = &after_arrow[first_quote + 1..];
            let Some(second_quote) = rest.find('"') else {
                return line.to_string();
            };
            count += 1;
            format!(
                "{}{}\"{}\"{}",
                before_value,
                &after_arrow[..first_quote],
                sha,
                &rest[second_quote + 1..]
            )
        })
        .collect::<Vec<_>>();
    if count != 1 {
        bail!("failed to update SHA256 for {target}");
    }
    Ok(preserve_trailing_newline(text, lines))
}

fn preserve_trailing_newline(original: &str, lines: Vec<String>) -> String {
    let mut out = lines.join("\n");
    if original.ends_with('\n') {
        out.push('\n');
    }
    out
}

trait StripSuffixAfter {
    fn strip_suffix_after(&self, needle: &str) -> Option<&str>;
}

impl StripSuffixAfter for str {
    fn strip_suffix_after(&self, needle: &str) -> Option<&str> {
        let index = self.find(needle)?;
        Some(&self[..index])
    }
}

fn validate_homebrew_repo_token(field: &str, value: &str) -> Result<()> {
    if value.trim().is_empty() {
        bail!("{field} cannot be empty");
    }
    if value.trim() != value {
        bail!("{field} cannot contain surrounding whitespace");
    }
    if !value
        .chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || matches!(ch, '-' | '_'))
    {
        bail!("{field} contains unsupported characters: {value}");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{ReleaseHomebrew, ReleaseNotes, ReleasePackage, ReleaseSdk, ReleaseTarget};

    #[test]
    fn replaces_single_homebrew_release_version() {
        let formula = "class Vo < Formula\n  release_version = \"0.1.0\"\nend\n";
        let updated = replace_release_version(formula, "0.2.0").unwrap();
        assert_eq!(
            updated,
            "class Vo < Formula\n  release_version = \"0.2.0\"\nend\n"
        );
    }

    #[test]
    fn rejects_missing_homebrew_release_version() {
        let error = replace_release_version("class Vo < Formula\nend\n", "0.2.0").unwrap_err();
        assert!(error.to_string().contains("release_version"));
    }

    #[test]
    fn homebrew_release_version_progression_is_monotonic_and_idempotent() {
        let formula = "class Vo < Formula\n  release_version = \"0.2.0\"\nend\n";
        validate_release_version_progression(formula, "0.2.0").unwrap();
        validate_release_version_progression(formula, "0.3.0").unwrap();

        let error = validate_release_version_progression(formula, "0.1.9").unwrap_err();
        assert!(error.to_string().contains("refusing to downgrade"));
    }

    #[test]
    fn classifies_concurrent_homebrew_release_progression() {
        let formula = "class Vo < Formula\n  release_version = \"0.2.0\"\nend\n";
        assert_eq!(
            homebrew_version_progression(formula, "0.1.9").unwrap(),
            HomebrewVersionProgression::Superseded
        );
        assert_eq!(
            homebrew_version_progression(formula, "0.2.0").unwrap(),
            HomebrewVersionProgression::Current
        );
        assert_eq!(
            homebrew_version_progression(formula, "0.3.0").unwrap(),
            HomebrewVersionProgression::Advance
        );
    }

    #[test]
    fn replaces_single_homebrew_target_sha() {
        let formula = concat!(
            "TARGET_SHA256 = {\n",
            "  \"aarch64-apple-darwin\" => \"old\",\n",
            "  \"x86_64-unknown-linux-gnu\" => \"keep\",\n",
            "}\n",
        );
        let updated =
            replace_formula_target_sha(formula, "aarch64-apple-darwin", "new-sha").unwrap();
        assert_eq!(
            updated,
            concat!(
                "TARGET_SHA256 = {\n",
                "  \"aarch64-apple-darwin\" => \"new-sha\",\n",
                "  \"x86_64-unknown-linux-gnu\" => \"keep\",\n",
                "}\n",
            )
        );
    }

    #[test]
    fn rejects_duplicate_homebrew_target_sha() {
        let formula = concat!(
            "TARGET_SHA256 = {\n",
            "  \"aarch64-apple-darwin\" => \"old-1\",\n",
            "  \"aarch64-apple-darwin\" => \"old-2\",\n",
            "}\n",
        );
        let error = replace_formula_target_sha(formula, "aarch64-apple-darwin", "new").unwrap_err();
        assert!(error.to_string().contains("aarch64-apple-darwin"));
    }

    #[test]
    fn homebrew_formula_targets_must_match_release_targets() {
        let formula = concat!(
            "class Vo < Formula\n",
            "  sha256_by_target = {\n",
            "    \"aarch64-apple-darwin\" => \"old\",\n",
            "  }\n",
            "end\n",
        );

        validate_homebrew_formula_targets(&sample_release(), formula).unwrap();
    }

    #[test]
    fn homebrew_formula_targets_rejects_extra_target() {
        let formula = concat!(
            "class Vo < Formula\n",
            "  sha256_by_target = {\n",
            "    \"aarch64-apple-darwin\" => \"old\",\n",
            "    \"old-target\" => \"old\",\n",
            "  }\n",
            "end\n",
        );

        let error = validate_homebrew_formula_targets(&sample_release(), formula).unwrap_err();
        assert!(error.to_string().contains("unexpected: old-target"));
    }

    fn sample_release() -> ReleaseFile {
        ReleaseFile {
            version: 2,
            package: ReleasePackage {
                crate_name: "vo".to_string(),
                binary: "vo".to_string(),
                artifact_prefix: "vo".to_string(),
                build_args: vec![
                    "--release".to_string(),
                    "--locked".to_string(),
                    "-p".to_string(),
                    "vo".to_string(),
                ],
                release_opt_level: "3".to_string(),
                release_lto: "thin".to_string(),
            },
            sdk: ReleaseSdk {
                registry: "crates-io".to_string(),
                internal_standalone: Vec::new(),
                packages: vec!["vo-common-core".to_string()],
            },
            notes: ReleaseNotes {
                product_name: "Vo".to_string(),
                homebrew: Vec::new(),
                manual_install: "Install manually.".to_string(),
            },
            homebrew: ReleaseHomebrew {
                repository: "vo-lang/homebrew-vo".to_string(),
                formula_path: "Formula/vo.rb".to_string(),
            },
            targets: vec![ReleaseTarget {
                target: "aarch64-apple-darwin".to_string(),
                os: "macos-14".to_string(),
            }],
        }
    }
}
