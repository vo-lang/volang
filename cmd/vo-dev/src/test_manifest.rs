use crate::test_config::load_test_config;
use anyhow::{anyhow, bail, Context, Result};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Deserialize)]
pub(crate) struct ManifestFile {
    pub(crate) version: u32,
    pub(crate) suite: String,
    pub(crate) root: String,
    #[serde(default, rename = "case")]
    pub(crate) cases: Vec<ManifestCase>,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct ManifestCase {
    pub(crate) id: String,
    pub(crate) kind: String,
    pub(crate) path: String,
    #[serde(default)]
    pub(crate) targets: Vec<String>,
    #[serde(default)]
    pub(crate) skip: Vec<String>,
    #[serde(default)]
    pub(crate) timeout: BTreeMap<String, u64>,
    reason: Option<String>,
    zip_root: Option<String>,
    #[serde(default)]
    blank: bool,
    expect: Option<toml::Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct CaseExpect {
    pub(crate) kind: String,
    pub(crate) patterns: Vec<String>,
    pub(crate) jit_regular_call_side_exits_min: Option<u64>,
}

pub(crate) fn load_manifest(root: &Path) -> Result<ManifestFile> {
    let path = root.join("tests/lang/manifest.toml");
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))
}

pub(crate) fn manifest_case_path(
    root: &Path,
    manifest: &ManifestFile,
    case: &ManifestCase,
) -> Result<String> {
    let base = root
        .join(&manifest.root)
        .join(case.path.trim_end_matches('/'));
    let path = if case.kind == "zip" {
        if let Some(zip_root) = &case.zip_root {
            format!("{}:{}", base.display(), zip_root)
        } else {
            base.display().to_string()
        }
    } else {
        base.display().to_string()
    };
    Ok(PathBuf::from(&path)
        .strip_prefix(root)
        .map(|p| p.display().to_string())
        .unwrap_or_else(|_| {
            if case.kind == "zip" && path.starts_with(&root.display().to_string()) {
                path[root.display().to_string().len() + 1..].to_string()
            } else {
                path
            }
        }))
}

pub(crate) fn parse_case_expect(case: &ManifestCase) -> Result<CaseExpect> {
    let Some(value) = &case.expect else {
        return Ok(CaseExpect {
            kind: "pass".to_string(),
            patterns: Vec::new(),
            jit_regular_call_side_exits_min: None,
        });
    };
    match value {
        toml::Value::String(s) if s == "pass" => Ok(CaseExpect {
            kind: "pass".to_string(),
            patterns: Vec::new(),
            jit_regular_call_side_exits_min: None,
        }),
        toml::Value::Table(table) => {
            if let Some(fail) = table.get("fail") {
                let patterns = match fail {
                    toml::Value::String(s) => vec![s.clone()],
                    toml::Value::Array(items) => items
                        .iter()
                        .map(|item| {
                            item.as_str().map(ToOwned::to_owned).ok_or_else(|| {
                                anyhow!("case {} fail patterns must be strings", case.id)
                            })
                        })
                        .collect::<Result<Vec<_>>>()?,
                    other => bail!(
                        "case {} fail expect must be string or array, got {other:?}",
                        case.id
                    ),
                };
                return Ok(CaseExpect {
                    kind: "fail".to_string(),
                    patterns,
                    jit_regular_call_side_exits_min: None,
                });
            }

            let kind = table
                .get("kind")
                .and_then(toml::Value::as_str)
                .unwrap_or("pass");
            if kind != "pass" {
                bail!("case {} has unsupported expect kind {kind}", case.id);
            }
            let jit_regular_call_side_exits_min = parse_u64_expect_min(
                case,
                table,
                "jit_regular_call_side_exits_min",
                Some("jit_regular_call_fallbacks_min"),
            )?;
            Ok(CaseExpect {
                kind: "pass".to_string(),
                patterns: Vec::new(),
                jit_regular_call_side_exits_min,
            })
        }
        other => bail!("case {} has invalid expect value {other:?}", case.id),
    }
}

fn parse_u64_expect_min(
    case: &ManifestCase,
    table: &toml::map::Map<String, toml::Value>,
    key: &'static str,
    legacy_key: Option<&'static str>,
) -> Result<Option<u64>> {
    let value = match (
        table.get(key),
        legacy_key.and_then(|legacy| table.get(legacy)),
    ) {
        (Some(_), Some(_)) => {
            bail!(
                "case {} must not set both {key} and legacy {legacy_key:?}",
                case.id
            )
        }
        (Some(value), None) | (None, Some(value)) => Some(value),
        (None, None) => None,
    };
    let Some(value) = value else {
        return Ok(None);
    };
    let raw = value
        .as_integer()
        .ok_or_else(|| anyhow!("case {} {key} must be an integer", case.id))?;
    Some(u64::try_from(raw).map_err(|_| anyhow!("case {} {key} must be non-negative", case.id)))
        .transpose()
}

pub(crate) fn lint_tests(root: &Path, suite: &str) -> Result<()> {
    let manifest = load_manifest(root)?;
    let test_config = load_test_config(root)?;
    let targets = &test_config.targets;
    let test_root = root.join(&manifest.root);
    if manifest.version != 1 {
        bail!("tests/lang/manifest.toml version must be 1");
    }
    if manifest.suite != "lang" {
        bail!("tests/lang/manifest.toml suite must be lang");
    }
    if manifest.suite != suite {
        bail!(
            "manifest suite {} does not match requested suite {}",
            manifest.suite,
            suite
        );
    }
    if manifest.root != "tests/lang" {
        bail!("tests/lang/manifest.toml root must be tests/lang");
    }
    let expected_keys = discover_manifest_case_keys(root)?;
    let actual_keys: BTreeSet<_> = manifest
        .cases
        .iter()
        .map(|case| (case.kind.clone(), case.path.clone()))
        .collect();
    if expected_keys != actual_keys {
        let missing: Vec<_> = expected_keys
            .difference(&actual_keys)
            .map(|(kind, path)| format!("{kind}:{path}"))
            .collect();
        let extra: Vec<_> = actual_keys
            .difference(&expected_keys)
            .map(|(kind, path)| format!("{kind}:{path}"))
            .collect();
        bail!(
            "tests/lang/manifest.toml is not in sync; missing=[{}] extra=[{}]",
            missing.join(", "),
            extra.join(", ")
        );
    }

    let mut ids = HashSet::new();
    for case in &manifest.cases {
        if !ids.insert(case.id.clone()) {
            bail!("duplicate test case id: {}", case.id);
        }
        validate_manifest_case_shape(case)?;
        if case.kind != "file" && case.kind != "project" && case.kind != "zip" {
            bail!("case {} has invalid kind {}", case.id, case.kind);
        }
        let expect = parse_case_expect(case)?;
        if case.targets.is_empty() && expect.kind != "fail" {
            bail!("case {} targets cannot be empty", case.id);
        }
        let mut target_names = HashSet::new();
        for target in &case.targets {
            if !target_names.insert(target) {
                bail!("case {} has duplicate target {}", case.id, target);
            }
        }
        let mut skip_names = HashSet::new();
        for target in &case.skip {
            if !skip_names.insert(target) {
                bail!("case {} has duplicate skip target {}", case.id, target);
            }
            if !target_names.contains(target) {
                bail!(
                    "case {} skips target {} that is not in targets",
                    case.id,
                    target
                );
            }
        }
        for target in case.targets.iter().chain(case.skip.iter()) {
            if !targets.contains_key(target) {
                bail!("case {} references unknown target {}", case.id, target);
            }
        }
        for (target, timeout_sec) in &case.timeout {
            if !targets.contains_key(target) {
                bail!("case {} has timeout for unknown target {}", case.id, target);
            }
            if !target_names.contains(target) {
                bail!(
                    "case {} has timeout for target {} that is not in targets",
                    case.id,
                    target
                );
            }
            if skip_names.contains(target) {
                bail!("case {} has timeout for skipped target {}", case.id, target);
            }
            if *timeout_sec == 0 {
                bail!("case {} timeout for target {} must be > 0", case.id, target);
            }
        }
        if !case.skip.is_empty() && !has_manifest_reason(case) {
            bail!("case {} has skips but no reason", case.id);
        }
        if !case.targets.is_empty()
            && case
                .targets
                .iter()
                .all(|target| has_target_name(&case.skip, target))
            && !case.blank
        {
            bail!(
                "case {} skips every declared target; remove it or mark it blank",
                case.id
            );
        }
        if case.targets.iter().any(|target| {
            targets
                .get(target)
                .is_some_and(|target| target.kind == "wasm")
        }) && case.kind != "file"
        {
            bail!(
                "case {} targets a wasm target but is not a file case",
                case.id
            );
        }
        let path = test_root.join(case.path.trim_end_matches('/'));
        match case.kind.as_str() {
            "file" if !path.is_file() => bail!("case {} missing file {}", case.id, case.path),
            "project" if !path.is_dir() => bail!("case {} missing project {}", case.id, case.path),
            "zip" if !path.is_file() => bail!("case {} missing zip {}", case.id, case.path),
            _ => {}
        }
        if expect.kind == "fail" && !case.targets.is_empty() {
            bail!(
                "case {} expected failure must not declare runtime targets",
                case.id
            );
        }
        if expect.kind == "fail" && expect.patterns.is_empty() {
            bail!(
                "case {} expected failure has no diagnostic patterns",
                case.id
            );
        }
        if expect.kind == "fail" && !has_manifest_reason(case) {
            bail!("case {} expected failure has no reason", case.id);
        }
        if expect.patterns.iter().any(|pattern| {
            pattern.trim().is_empty() || pattern.contains("TODO_EXPECTED_DIAGNOSTIC")
        }) {
            bail!("case {} contains empty/TODO failure diagnostic", case.id);
        }
        if case.expect.is_none() {
            bail!("case {} must declare expect", case.id);
        }
        if case.blank && case.skip.is_empty() {
            bail!("blank case {} must be explicitly skipped", case.id);
        }
        if case.kind == "file" && expect.kind == "pass" && !case.blank {
            let missing_default_targets =
                missing_required_targets(&case.targets, &test_config.required_file_pass_targets);
            if !missing_default_targets.is_empty()
                && !looks_like_gc_regression(case)
                && !has_manifest_reason(case)
            {
                bail!(
                    "case {} omits default file target(s) {} but has no reason",
                    case.id,
                    missing_default_targets.join(",")
                );
            }
            if looks_like_gc_regression(case)
                && !missing_required_targets(&case.targets, &test_config.gc_regression_targets)
                    .is_empty()
            {
                bail!(
                    "case {} looks like a GC regression but does not target {}",
                    case.id,
                    test_config.gc_regression_targets.join(",")
                );
            }
        }
        if case.kind == "file" {
            let is_blank = fs::read_to_string(&path)
                .with_context(|| format!("could not read test case {}", case.path))?
                .trim()
                .is_empty();
            if is_blank && !case.blank {
                bail!("blank file case {} must set blank = true", case.id);
            }
            if case.blank && !is_blank {
                bail!("case {} sets blank = true but file is not blank", case.id);
            }
            if case.blank {
                for target in &case.targets {
                    if !has_target_name(&case.skip, target) {
                        bail!(
                            "blank case {} must skip target {} or remove it from targets",
                            case.id,
                            target
                        );
                    }
                }
            }
        }
    }
    Ok(())
}

fn validate_manifest_case_shape(case: &ManifestCase) -> Result<()> {
    if case.id.trim().is_empty() {
        bail!("case id cannot be empty");
    }
    if case.id.trim() != case.id {
        bail!("case {} id cannot contain surrounding whitespace", case.id);
    }
    if !case
        .id
        .chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || matches!(ch, '.' | '_' | '-'))
    {
        bail!(
            "case {} id must use only lowercase ASCII letters, digits, dot, underscore, or hyphen",
            case.id
        );
    }
    validate_manifest_relative_path(&case.id, "path", &case.path, true)?;
    match case.kind.as_str() {
        "file" | "zip" if case.path.ends_with('/') => {
            bail!("case {} {} path cannot end with /", case.id, case.kind)
        }
        "project" if !case.path.ends_with('/') => {
            bail!("case {} project path must end with /", case.id)
        }
        _ => {}
    }
    match (&case.kind[..], &case.zip_root) {
        ("zip", Some(zip_root)) => {
            validate_manifest_relative_path(&case.id, "zip_root", zip_root, true)?;
        }
        ("zip", None) => {}
        (_, Some(_)) => bail!("case {} zip_root is only valid for zip cases", case.id),
        (_, None) => {}
    }
    Ok(())
}

fn validate_manifest_relative_path(
    case_id: &str,
    field: &str,
    value: &str,
    allow_trailing_slash: bool,
) -> Result<()> {
    if value.trim().is_empty() {
        bail!("case {case_id} {field} cannot be empty");
    }
    if value.trim() != value {
        bail!("case {case_id} {field} cannot contain surrounding whitespace");
    }
    if value.contains('\\') {
        bail!("case {case_id} {field} must use / path separators");
    }
    if Path::new(value).is_absolute() {
        bail!("case {case_id} {field} must be relative");
    }
    if value.contains("//") {
        bail!("case {case_id} {field} cannot contain empty path segments");
    }
    let trimmed = if allow_trailing_slash {
        value.trim_end_matches('/')
    } else {
        value
    };
    if trimmed.is_empty() {
        bail!("case {case_id} {field} cannot point to repository root");
    }
    for segment in trimmed.split('/') {
        if segment.is_empty() || segment == "." || segment == ".." {
            bail!("case {case_id} {field} contains invalid segment {segment:?}");
        }
    }
    Ok(())
}

pub(crate) fn has_target_name(values: &[String], target: &str) -> bool {
    values.iter().any(|value| value == target)
}

fn has_manifest_reason(case: &ManifestCase) -> bool {
    !case
        .reason
        .as_deref()
        .map(str::trim)
        .unwrap_or_default()
        .is_empty()
}

fn missing_required_targets(targets: &[String], required: &[String]) -> Vec<String> {
    required
        .iter()
        .filter(|target| !has_target_name(targets, target))
        .cloned()
        .collect()
}

fn looks_like_gc_regression(case: &ManifestCase) -> bool {
    let id = case.id.as_str();
    let path = case.path.as_str();
    id.starts_with("gc-")
        || id.contains(".gc-")
        || id.contains("-gc")
        || id.contains("_gc")
        || path.starts_with("gc_")
        || path.contains("/gc_")
        || path.contains("_gc")
}

fn discover_manifest_case_keys(root: &Path) -> Result<BTreeSet<(String, String)>> {
    let test_root = root.join("tests/lang");
    let mut keys = BTreeSet::new();
    for file in collect_vo_files(&test_root, true)? {
        keys.insert(("file".to_string(), rel_to_test_data(root, &file)?));
    }
    for dir in collect_project_dirs(&test_root)? {
        let mut rel = rel_to_test_data(root, &dir)?;
        rel.push('/');
        keys.insert(("project".to_string(), rel));
    }
    for file in collect_zip_files(&test_root)? {
        keys.insert(("zip".to_string(), rel_to_test_data(root, &file)?));
    }
    Ok(keys)
}

fn collect_vo_files(root: &Path, skip_project_dirs: bool) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    collect_vo_files_inner(root, skip_project_dirs, &mut out)?;
    out.sort();
    Ok(out)
}

fn collect_vo_files_inner(
    dir: &Path,
    skip_project_dirs: bool,
    out: &mut Vec<PathBuf>,
) -> Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.starts_with('.') {
            continue;
        }
        if path.is_dir() {
            if skip_project_dirs && name.starts_with("proj_") {
                continue;
            }
            collect_vo_files_inner(&path, skip_project_dirs, out)?;
        } else if path.extension().and_then(|s| s.to_str()) == Some("vo") {
            out.push(path);
        }
    }
    Ok(())
}

fn collect_project_dirs(root: &Path) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    collect_project_dirs_inner(root, &mut out)?;
    out.sort();
    Ok(out)
}

fn collect_project_dirs_inner(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if !path.is_dir() || name.starts_with('.') {
            continue;
        }
        if name.starts_with("proj_") {
            out.push(path);
        } else {
            collect_project_dirs_inner(&path, out)?;
        }
    }
    Ok(())
}

fn collect_zip_files(root: &Path) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    collect_zip_files_inner(root, &mut out)?;
    out.sort();
    Ok(out)
}

fn collect_zip_files_inner(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let name = entry.file_name();
        if name.to_string_lossy().starts_with('.') {
            continue;
        }
        if path.is_dir() {
            collect_zip_files_inner(&path, out)?;
        } else if path.extension().and_then(|s| s.to_str()) == Some("zip") {
            out.push(path);
        }
    }
    Ok(())
}

fn rel_to_test_data(root: &Path, path: &Path) -> Result<String> {
    Ok(path
        .strip_prefix(root.join("tests/lang"))?
        .to_string_lossy()
        .trim_start_matches('/')
        .to_string())
}
