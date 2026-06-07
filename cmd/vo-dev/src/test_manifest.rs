use crate::test_config::{load_test_config, TestConfig};
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
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
    pub(crate) matrix: Option<String>,
    #[serde(default)]
    pub(crate) tags: Vec<String>,
    pub(crate) owner: Option<String>,
    #[serde(default)]
    pub(crate) skip: Vec<String>,
    #[serde(default)]
    pub(crate) timeout: BTreeMap<String, u64>,
    pub(crate) reason: Option<String>,
    pub(crate) zip_root: Option<String>,
    #[serde(default)]
    pub(crate) blank: bool,
    pub(crate) expect: Option<toml::Value>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct CaseExpect {
    pub(crate) kind: String,
    pub(crate) patterns: Vec<String>,
    pub(crate) jit_regular_call_side_exits_min: Option<u64>,
}

#[derive(Debug, Serialize)]
struct TestCatalog {
    schema: &'static str,
    suite: String,
    cases: Vec<TestCatalogCase>,
}

#[derive(Debug, Serialize)]
struct TestCatalogCase {
    id: String,
    kind: String,
    path: String,
    targets: Vec<String>,
    matrix: Option<String>,
    tags: Vec<String>,
    owner: Option<String>,
    skip: Vec<String>,
    timeout: BTreeMap<String, u64>,
    reason: Option<String>,
    expect: CaseExpect,
}

#[derive(Debug, Serialize)]
struct TestStats {
    schema: &'static str,
    suite: String,
    cases: usize,
    jobs: usize,
    expected_fail_cases: usize,
    skip_entries: usize,
    cases_by_kind: BTreeMap<String, usize>,
    cases_by_matrix: BTreeMap<String, usize>,
    jobs_by_target: BTreeMap<String, usize>,
    skips_by_target: BTreeMap<String, usize>,
    cases_by_tag: BTreeMap<String, usize>,
    cases_by_owner: BTreeMap<String, usize>,
}

#[derive(Debug, Serialize)]
struct TestCoverage {
    schema: &'static str,
    suite: String,
    cases: usize,
    missing_matrix: usize,
    missing_tags: usize,
    missing_owner: usize,
    missing_expect: usize,
    explicit_targets: usize,
    explicit_targets_without_reason: usize,
    compile_fail_without_compile_matrix: usize,
    gc_filename_only: usize,
    unowned_skips: usize,
    unowned_failures: usize,
    unowned_timeouts: usize,
    unowned_target_overrides: usize,
}

#[derive(Debug, Serialize)]
struct TestExplain {
    schema: &'static str,
    suite: String,
    case: TestCatalogCase,
    selected_targets: Vec<String>,
    selected_jobs: Vec<String>,
    reasons: Vec<String>,
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

pub(crate) fn resolved_case_targets(
    case: &ManifestCase,
    test_config: &TestConfig,
) -> Result<Vec<String>> {
    if let Some(matrix) = case.matrix.as_deref() {
        if !test_config.matrices.contains_key(matrix) {
            bail!("case {} references unknown matrix {}", case.id, matrix);
        }
    }
    if !case.targets.is_empty() {
        return Ok(case.targets.clone());
    }
    let Some(matrix) = case.matrix.as_deref() else {
        return Ok(Vec::new());
    };
    test_config
        .matrices
        .get(matrix)
        .cloned()
        .ok_or_else(|| anyhow!("case {} references unknown matrix {}", case.id, matrix))
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

pub(crate) fn lint_tests(root: &Path, suite: &str, strict: bool) -> Result<()> {
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
        validate_manifest_case_metadata(case, &test_config, strict)?;
        if case.kind != "file" && case.kind != "project" && case.kind != "zip" {
            bail!("case {} has invalid kind {}", case.id, case.kind);
        }
        let expect = parse_case_expect(case)?;
        let resolved_targets = resolved_case_targets(case, &test_config)?;
        if resolved_targets.is_empty() && expect.kind != "fail" {
            bail!("case {} must declare matrix or targets", case.id);
        }
        let mut target_names = HashSet::new();
        for target in &resolved_targets {
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
        for target in resolved_targets.iter().chain(case.skip.iter()) {
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
        if !resolved_targets.is_empty()
            && resolved_targets
                .iter()
                .all(|target| has_target_name(&case.skip, target))
            && !case.blank
        {
            bail!(
                "case {} skips every declared target; remove it or mark it blank",
                case.id
            );
        }
        if resolved_targets.iter().any(|target| {
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
        if expect.kind == "fail"
            && resolved_targets
                .iter()
                .any(|target| target.as_str() != "compile")
        {
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
            let missing_default_targets = missing_required_targets(
                &resolved_targets,
                &test_config.required_file_pass_targets,
            );
            if !missing_default_targets.is_empty()
                && !is_gc_regression_case(case)
                && !has_manifest_reason(case)
            {
                bail!(
                    "case {} omits default file target(s) {} but has no reason",
                    case.id,
                    missing_default_targets.join(",")
                );
            }
            if is_gc_regression_case(case)
                && !missing_required_targets(&resolved_targets, &test_config.gc_regression_targets)
                    .is_empty()
                && !has_manifest_reason(case)
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
                for target in &resolved_targets {
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

pub(crate) fn print_test_stats(root: &Path, suite: &str, format: &str) -> Result<()> {
    let stats = collect_test_stats(root, suite)?;
    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&stats)?);
    } else {
        print_test_stats_text(&stats);
    }
    Ok(())
}

fn collect_test_stats(root: &Path, suite: &str) -> Result<TestStats> {
    let manifest = load_manifest(root)?;
    let test_config = load_test_config(root)?;
    if manifest.suite != suite {
        bail!(
            "manifest suite {} does not match requested suite {}",
            manifest.suite,
            suite
        );
    }

    let mut case_count = 0usize;
    let mut job_count = 0usize;
    let mut expected_fail_count = 0usize;
    let mut skip_count = 0usize;
    let mut by_kind = BTreeMap::new();
    let mut by_matrix = BTreeMap::new();
    let mut jobs_by_target = BTreeMap::new();
    let mut skips_by_target = BTreeMap::new();
    let mut by_tag = BTreeMap::new();
    let mut by_owner = BTreeMap::new();

    for case in &manifest.cases {
        case_count += 1;
        increment(&mut by_kind, &case.kind);
        increment(
            &mut by_matrix,
            case.matrix
                .as_deref()
                .unwrap_or(if case.targets.is_empty() {
                    "(none)"
                } else {
                    "(explicit-targets)"
                }),
        );
        increment(&mut by_owner, case.owner.as_deref().unwrap_or("(missing)"));
        if case.tags.is_empty() {
            increment(&mut by_tag, "(missing)");
        } else {
            for tag in &case.tags {
                increment(&mut by_tag, tag);
            }
        }

        let expect = parse_case_expect(case)?;
        let targets = resolved_case_targets(case, &test_config)?;
        if expect.kind == "fail" {
            expected_fail_count += 1;
            if compile_fail_enabled_for_stats(&targets) {
                job_count += 1;
                increment(&mut jobs_by_target, "compile");
            }
        } else {
            for target in &targets {
                if has_target_name(&case.skip, target) {
                    continue;
                }
                job_count += 1;
                increment(&mut jobs_by_target, target);
            }
        }

        skip_count += case.skip.len();
        for target in &case.skip {
            increment(&mut skips_by_target, target);
        }
    }

    Ok(TestStats {
        schema: "volang.test-stats.v1",
        suite: manifest.suite,
        cases: case_count,
        jobs: job_count,
        expected_fail_cases: expected_fail_count,
        skip_entries: skip_count,
        cases_by_kind: by_kind,
        cases_by_matrix: by_matrix,
        jobs_by_target,
        skips_by_target,
        cases_by_tag: by_tag,
        cases_by_owner: by_owner,
    })
}

fn print_test_stats_text(stats: &TestStats) {
    println!("{} test stats:", stats.suite);
    println!("  cases: {}", stats.cases);
    println!("  jobs: {}", stats.jobs);
    println!("  expected-fail cases: {}", stats.expected_fail_cases);
    println!("  skip entries: {}", stats.skip_entries);
    print_count_map("cases by kind", &stats.cases_by_kind);
    print_count_map("cases by matrix", &stats.cases_by_matrix);
    print_count_map("jobs by target", &stats.jobs_by_target);
    print_count_map("skips by target", &stats.skips_by_target);
    print_count_map("cases by tag", &stats.cases_by_tag);
    print_count_map("cases by owner", &stats.cases_by_owner);
}

pub(crate) fn print_test_catalog(root: &Path, suite: &str, format: &str) -> Result<()> {
    let manifest = load_manifest(root)?;
    let test_config = load_test_config(root)?;
    if manifest.suite != suite {
        bail!(
            "manifest suite {} does not match requested suite {}",
            manifest.suite,
            suite
        );
    }
    if format != "text" && format != "json" {
        bail!("--format must be text or json");
    }

    let mut cases = Vec::new();
    for case in &manifest.cases {
        cases.push(TestCatalogCase {
            id: case.id.clone(),
            kind: case.kind.clone(),
            path: manifest_case_path(root, &manifest, case)?,
            targets: resolved_case_targets(case, &test_config)?,
            matrix: case.matrix.clone(),
            tags: case.tags.clone(),
            owner: case.owner.clone(),
            skip: case.skip.clone(),
            timeout: case.timeout.clone(),
            reason: case.reason.clone(),
            expect: parse_case_expect(case)?,
        });
    }

    if format == "json" {
        println!(
            "{}",
            serde_json::to_string_pretty(&TestCatalog {
                schema: "volang.test-catalog.v1",
                suite: manifest.suite,
                cases,
            })?
        );
    } else {
        for case in cases {
            println!(
                "{}\t{}\t{}\t{}\t{}\t{}",
                case.id,
                case.kind,
                case.matrix.as_deref().unwrap_or("(explicit-targets)"),
                case.targets.join(","),
                case.owner.as_deref().unwrap_or("(missing)"),
                case.path
            );
        }
    }
    Ok(())
}

pub(crate) fn print_test_coverage(root: &Path, suite: &str, format: &str) -> Result<()> {
    let coverage = collect_test_coverage(root, suite)?;
    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&coverage)?);
    } else {
        println!("{} test metadata coverage:", coverage.suite);
        println!("  cases: {}", coverage.cases);
        println!("  missing matrix: {}", coverage.missing_matrix);
        println!("  missing tags: {}", coverage.missing_tags);
        println!("  missing owner: {}", coverage.missing_owner);
        println!("  missing expect: {}", coverage.missing_expect);
        println!("  explicit targets: {}", coverage.explicit_targets);
        println!(
            "  explicit targets without reason: {}",
            coverage.explicit_targets_without_reason
        );
        println!(
            "  compile-fail without compile matrix: {}",
            coverage.compile_fail_without_compile_matrix
        );
        println!("  filename-only GC metadata: {}", coverage.gc_filename_only);
        println!("  unowned skips: {}", coverage.unowned_skips);
        println!("  unowned failures: {}", coverage.unowned_failures);
        println!("  unowned timeouts: {}", coverage.unowned_timeouts);
        println!(
            "  unowned target overrides: {}",
            coverage.unowned_target_overrides
        );
    }
    if coverage.missing_matrix != 0
        || coverage.missing_tags != 0
        || coverage.missing_owner != 0
        || coverage.missing_expect != 0
        || coverage.explicit_targets != 0
        || coverage.explicit_targets_without_reason != 0
        || coverage.compile_fail_without_compile_matrix != 0
        || coverage.gc_filename_only != 0
        || coverage.unowned_skips != 0
        || coverage.unowned_failures != 0
        || coverage.unowned_timeouts != 0
        || coverage.unowned_target_overrides != 0
    {
        bail!("{} test metadata coverage is incomplete", coverage.suite);
    }
    Ok(())
}

pub(crate) fn explain_test_case(
    root: &Path,
    suite: &str,
    case_id: &str,
    format: &str,
) -> Result<()> {
    let manifest = load_manifest(root)?;
    let test_config = load_test_config(root)?;
    if manifest.suite != suite {
        bail!(
            "manifest suite {} does not match requested suite {}",
            manifest.suite,
            suite
        );
    }
    let case = manifest
        .cases
        .iter()
        .find(|case| case.id == case_id)
        .ok_or_else(|| anyhow!("unknown test case: {case_id}"))?;
    let selected_targets = resolved_case_targets(case, &test_config)?;
    let expect = parse_case_expect(case)?;
    let selected_jobs = selected_targets
        .iter()
        .filter(|target| !has_target_name(&case.skip, target))
        .map(|target| {
            if expect.kind == "fail" {
                format!("{}::{target}-compile", case.id)
            } else {
                format!("{}::{target}", case.id)
            }
        })
        .collect::<Vec<_>>();
    let mut reasons = Vec::new();
    if let Some(matrix) = &case.matrix {
        reasons.push(format!(
            "matrix {matrix} expands to {}",
            selected_targets.join(",")
        ));
    }
    if !case.targets.is_empty() {
        reasons.push(format!(
            "explicit target override {}",
            case.targets.join(",")
        ));
    }
    if !case.tags.is_empty() {
        reasons.push(format!("tags {}", case.tags.join(",")));
    }
    if let Some(owner) = &case.owner {
        reasons.push(format!("owner {owner}"));
    }
    if !case.skip.is_empty() {
        reasons.push(format!(
            "skips {} because {}",
            case.skip.join(","),
            case.reason.as_deref().unwrap_or("(missing reason)")
        ));
    }
    if !case.timeout.is_empty() {
        reasons.push(format!("target timeouts {:?}", case.timeout));
    }
    if expect.kind == "fail" {
        reasons.push(format!(
            "expected compile failure because {}",
            case.reason.as_deref().unwrap_or("(missing reason)")
        ));
    }
    let explanation = TestExplain {
        schema: "volang.test-explain.v1",
        suite: manifest.suite.clone(),
        case: TestCatalogCase {
            id: case.id.clone(),
            kind: case.kind.clone(),
            path: manifest_case_path(root, &manifest, case)?,
            targets: selected_targets.clone(),
            matrix: case.matrix.clone(),
            tags: case.tags.clone(),
            owner: case.owner.clone(),
            skip: case.skip.clone(),
            timeout: case.timeout.clone(),
            reason: case.reason.clone(),
            expect,
        },
        selected_targets,
        selected_jobs,
        reasons,
    };
    if format == "json" {
        println!("{}", serde_json::to_string_pretty(&explanation)?);
    } else {
        println!("test case {}", explanation.case.id);
        println!("  path: {}", explanation.case.path);
        println!(
            "  matrix: {}",
            explanation.case.matrix.as_deref().unwrap_or("(none)")
        );
        println!(
            "  owner: {}",
            explanation.case.owner.as_deref().unwrap_or("(missing)")
        );
        println!("  tags: {}", explanation.case.tags.join(","));
        println!("  targets: {}", explanation.selected_targets.join(","));
        println!("  jobs: {}", explanation.selected_jobs.join(","));
        println!("  reasons:");
        for reason in &explanation.reasons {
            println!("    - {reason}");
        }
    }
    Ok(())
}

fn collect_test_coverage(root: &Path, suite: &str) -> Result<TestCoverage> {
    let manifest = load_manifest(root)?;
    if manifest.suite != suite {
        bail!(
            "manifest suite {} does not match requested suite {}",
            manifest.suite,
            suite
        );
    }
    let mut missing_matrix = 0usize;
    let mut missing_tags = 0usize;
    let mut missing_owner = 0usize;
    let mut missing_expect = 0usize;
    let mut explicit_targets = 0usize;
    let mut explicit_targets_without_reason = 0usize;
    let mut compile_fail_without_compile_matrix = 0usize;
    let mut gc_filename_only = 0usize;
    let mut unowned_skips = 0usize;
    let mut unowned_failures = 0usize;
    let mut unowned_timeouts = 0usize;
    let mut unowned_target_overrides = 0usize;
    for case in &manifest.cases {
        let has_owner = !case.owner.as_deref().unwrap_or_default().trim().is_empty();
        if case.matrix.is_none() {
            missing_matrix += 1;
        }
        if case.tags.is_empty() {
            missing_tags += 1;
        }
        if !has_owner {
            missing_owner += 1;
        }
        if case.expect.is_none() {
            missing_expect += 1;
        }
        if !case.targets.is_empty() {
            explicit_targets += 1;
            if !has_manifest_reason(case) {
                explicit_targets_without_reason += 1;
            }
            if !has_owner {
                unowned_target_overrides += 1;
            }
        }
        let expect = parse_case_expect(case)?;
        if expect.kind == "fail" {
            if case.matrix.as_deref() != Some("compile") {
                compile_fail_without_compile_matrix += 1;
            }
            if !has_owner {
                unowned_failures += 1;
            }
        }
        if looks_like_gc_regression(case)
            && case.matrix.as_deref() != Some("gc")
            && !case.tags.iter().any(|tag| tag == "gc")
        {
            gc_filename_only += 1;
        }
        if !case.skip.is_empty() && !has_owner {
            unowned_skips += 1;
        }
        if !case.timeout.is_empty() && !has_owner {
            unowned_timeouts += 1;
        }
    }
    Ok(TestCoverage {
        schema: "volang.test-coverage.v1",
        suite: manifest.suite,
        cases: manifest.cases.len(),
        missing_matrix,
        missing_tags,
        missing_owner,
        missing_expect,
        explicit_targets,
        explicit_targets_without_reason,
        compile_fail_without_compile_matrix,
        gc_filename_only,
        unowned_skips,
        unowned_failures,
        unowned_timeouts,
        unowned_target_overrides,
    })
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
    if let Some(matrix) = &case.matrix {
        validate_manifest_label(&case.id, "matrix", matrix)?;
    }
    let mut seen_tags = BTreeSet::new();
    for tag in &case.tags {
        validate_manifest_label(&case.id, "tag", tag)?;
        if !seen_tags.insert(tag) {
            bail!("case {} has duplicate tag {}", case.id, tag);
        }
    }
    if let Some(owner) = &case.owner {
        validate_manifest_label(&case.id, "owner", owner)?;
    }
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

fn validate_manifest_case_metadata(
    case: &ManifestCase,
    test_config: &TestConfig,
    strict: bool,
) -> Result<()> {
    if !strict {
        if !case.targets.is_empty() {
            for (matrix, targets) in &test_config.matrices {
                if targets == &case.targets {
                    eprintln!(
                        "warning: case {} explicit targets match matrix {}; prefer matrix = {:?}",
                        case.id, matrix, matrix
                    );
                    break;
                }
            }
        }
        return Ok(());
    }
    if case.matrix.as_deref().unwrap_or_default().trim().is_empty() {
        bail!(
            "case {} missing matrix; add matrix = \"default\", \"native\", \"gc\", or \"compile\"",
            case.id
        );
    }
    if case.tags.is_empty() {
        bail!(
            "case {} missing tags; add tags = [\"<domain>\", \"regression\"]",
            case.id
        );
    }
    if case.owner.as_deref().unwrap_or_default().trim().is_empty() {
        bail!(
            "case {} missing owner; add owner = \"<subsystem>\"",
            case.id
        );
    }
    if case.expect.is_none() {
        bail!(
            "case {} missing expect; add expect = \"pass\" or expect = {{ fail = [...] }}",
            case.id
        );
    }
    if !case.targets.is_empty() {
        bail!(
            "case {} uses explicit targets; completion requires a named matrix in eng/tests.toml",
            case.id
        );
    }
    if !case.timeout.is_empty() && !has_manifest_reason(case) {
        bail!("case {} has timeout but no reason", case.id);
    }
    let expect = parse_case_expect(case)?;
    if expect.kind == "fail" && case.matrix.as_deref() != Some("compile") {
        bail!(
            "case {} expected failure must use matrix = \"compile\"",
            case.id
        );
    }
    if expect.kind == "fail" && !case.tags.iter().any(|tag| tag == "compile-fail") {
        bail!(
            "case {} expected failure must include tag compile-fail",
            case.id
        );
    }
    if case.blank
        && !matches!(
            case.owner.as_deref(),
            Some("eng") | Some("runtime") | Some("stdlib") | Some("module") | Some("typechecker")
        )
    {
        bail!(
            "blank case {} must be owned by eng or the relevant subsystem",
            case.id
        );
    }
    if looks_like_gc_regression(case)
        && case.matrix.as_deref() != Some("gc")
        && !case.tags.iter().any(|tag| tag == "gc")
    {
        bail!(
            "case {} looks like a GC regression but is not selected by matrix/tag metadata",
            case.id
        );
    }
    for (matrix, targets) in &test_config.matrices {
        if targets == &case.targets {
            bail!(
                "case {} explicit targets match matrix {}; use matrix = {:?}",
                case.id,
                matrix,
                matrix
            );
        }
    }
    Ok(())
}

fn validate_manifest_label(case_id: &str, field: &str, value: &str) -> Result<()> {
    if value.trim().is_empty() {
        bail!("case {case_id} {field} cannot be empty");
    }
    if value.trim() != value {
        bail!("case {case_id} {field} cannot contain surrounding whitespace");
    }
    if !value
        .chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || matches!(ch, '-' | '_' | '.'))
    {
        bail!(
            "case {case_id} {field} {value} must use lowercase ASCII letters, digits, dot, hyphen, or underscore"
        );
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

fn compile_fail_enabled_for_stats(targets: &[String]) -> bool {
    targets.is_empty() || targets.iter().any(|target| target == "compile")
}

fn increment(counts: &mut BTreeMap<String, usize>, key: &str) {
    *counts.entry(key.to_string()).or_default() += 1;
}

fn print_count_map(title: &str, counts: &BTreeMap<String, usize>) {
    println!("  {title}:");
    if counts.is_empty() {
        println!("    (none): 0");
        return;
    }
    for (key, count) in counts {
        println!("    {key}: {count}");
    }
}

fn is_gc_regression_case(case: &ManifestCase) -> bool {
    case.matrix.as_deref() == Some("gc")
        || case.tags.iter().any(|tag| tag == "gc")
        || looks_like_gc_regression(case)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stats_json_schema_is_stable() {
        let stats = TestStats {
            schema: "volang.test-stats.v1",
            suite: "lang".to_string(),
            cases: 1,
            jobs: 1,
            expected_fail_cases: 0,
            skip_entries: 0,
            cases_by_kind: BTreeMap::new(),
            cases_by_matrix: BTreeMap::new(),
            jobs_by_target: BTreeMap::new(),
            skips_by_target: BTreeMap::new(),
            cases_by_tag: BTreeMap::new(),
            cases_by_owner: BTreeMap::new(),
        };
        let value = serde_json::to_value(stats).unwrap();
        assert_eq!(value["schema"], "volang.test-stats.v1");
        assert!(value.get("cases_by_matrix").is_some());
        assert!(value.get("jobs_by_target").is_some());
    }
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
