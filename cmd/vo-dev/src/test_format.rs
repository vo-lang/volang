use crate::test_manifest::{load_manifest, parse_case_expect, ManifestCase};
use anyhow::{bail, Context, Result};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) struct TestFormatSummary {
    pub(crate) checked: usize,
    pub(crate) skipped_parser_negative: usize,
}

pub(crate) fn check_test_formatting(root: &Path, suite: &str) -> Result<TestFormatSummary> {
    let manifest = load_manifest(root)?;
    if manifest.suite != suite {
        bail!(
            "manifest suite {} does not match requested suite {}",
            manifest.suite,
            suite
        );
    }

    let mut sources = BTreeMap::<PathBuf, bool>::new();
    for directory in [
        "lang/stdlib",
        "examples",
        "benchmarks",
        "apps/studio/src/assets/examples",
        "apps/playground-legacy/src/assets/examples",
    ] {
        collect_vo_files(&root.join(directory), false, &mut sources)?;
    }
    let test_root = root.join(&manifest.root);
    for case in &manifest.cases {
        let path = test_root.join(case.path.trim_end_matches('/'));
        let allow_parse_failure = is_parser_negative(case)?;
        match case.kind.as_str() {
            "file" => add_source(&mut sources, path, allow_parse_failure),
            "project" => collect_vo_files(&path, allow_parse_failure, &mut sources)?,
            "zip" => {}
            _ => bail!("case {} has invalid kind {}", case.id, case.kind),
        }
    }

    let mut checked = 0;
    let mut skipped_parser_negative = 0;
    let mut failures = Vec::new();
    for (path, allow_parse_failure) in sources {
        let source = fs::read_to_string(&path)
            .with_context(|| format!("could not read {}", path.display()))?;
        match vo_engine::format_source(&source) {
            Ok(formatted) => {
                checked += 1;
                if formatted != source {
                    failures.push(format!(
                        "unformatted Vo source: {}",
                        display_path(root, &path)
                    ));
                }
            }
            Err(_) if allow_parse_failure => skipped_parser_negative += 1,
            Err(error) => failures.push(format!(
                "Vo formatter rejected {}: {}",
                display_path(root, &path),
                error
            )),
        }
    }

    if !failures.is_empty() {
        bail!("Vo formatting check failed:\n{}", failures.join("\n"));
    }
    Ok(TestFormatSummary {
        checked,
        skipped_parser_negative,
    })
}

fn is_parser_negative(case: &ManifestCase) -> Result<bool> {
    Ok(parse_case_expect(case)?.kind == "fail" && case.tags.iter().any(|tag| tag == "parser"))
}

fn collect_vo_files(
    directory: &Path,
    allow_parse_failure: bool,
    sources: &mut BTreeMap<PathBuf, bool>,
) -> Result<()> {
    let mut entries = fs::read_dir(directory)
        .with_context(|| format!("could not read {}", directory.display()))?
        .collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.file_name());
    for entry in entries {
        let file_type = entry.file_type()?;
        if file_type.is_symlink() {
            continue;
        }
        let path = entry.path();
        if file_type.is_dir() {
            collect_vo_files(&path, allow_parse_failure, sources)?;
        } else if file_type.is_file() && path.extension().is_some_and(|ext| ext == "vo") {
            add_source(sources, path, allow_parse_failure);
        }
    }
    Ok(())
}

fn add_source(sources: &mut BTreeMap<PathBuf, bool>, path: PathBuf, allow_parse_failure: bool) {
    sources
        .entry(path)
        .and_modify(|allowed| *allowed &= allow_parse_failure)
        .or_insert(allow_parse_failure);
}

fn display_path(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .into_owned()
}

#[cfg(test)]
mod tests {
    use super::add_source;
    use std::collections::BTreeMap;
    use std::path::PathBuf;

    #[test]
    fn duplicate_path_is_skipped_only_when_every_case_allows_parse_failure() {
        let path = PathBuf::from("case.vo");
        let mut sources = BTreeMap::new();
        add_source(&mut sources, path.clone(), true);
        add_source(&mut sources, path.clone(), false);
        assert_eq!(sources.get(&path), Some(&false));
    }
}
