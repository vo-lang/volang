use crate::artifact_lint::lint_artifacts;
use crate::config::{load_project, ProjectRepo};
use crate::lint_policy::{validate_ascii_slug, validate_repo_path_like};
use crate::release_system;
use anyhow::{anyhow, bail, Context, Result};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;

const ALL_LINT_TARGETS: &[&str] = &[
    "artifacts",
    "repo-boundaries",
    "layout",
    "docs",
    "skill",
    "examples",
    "benchmarks",
    "release",
    "ci",
];

pub(crate) fn cmd_lint(root: &Path, args: Vec<String>) -> Result<()> {
    let opts = LintArgs::parse(args)?;
    let target = opts.target.as_str();
    if target == "all" {
        for target in ALL_LINT_TARGETS {
            run_lint_target(root, target)?;
        }
        println!("vo-dev lint all: ok");
        return Ok(());
    }
    run_lint_target(root, target)?;
    println!("vo-dev lint {target}: ok");
    Ok(())
}

fn run_lint_target(root: &Path, target: &str) -> Result<()> {
    match target {
        "artifacts" => lint_artifacts(root)?,
        "repo-boundaries" => lint_repo_boundaries(root)?,
        "layout" => lint_layout(root)?,
        "docs" => lint_docs(root)?,
        "skill" => lint_volang_skill(root)?,
        "examples" => lint_examples(root)?,
        "benchmarks" => lint_benchmarks(root)?,
        "release" => release_system::lint_release(root)?,
        "ci" => crate::ci::cmd_ci(root, vec!["lint".to_string()])?,
        other => bail!("unknown lint target: {other}"),
    }
    Ok(())
}

struct LintArgs {
    target: String,
}

impl LintArgs {
    fn parse(args: Vec<String>) -> Result<Self> {
        let mut target = "all".to_string();
        let mut target_seen = false;
        for arg in args {
            match arg.as_str() {
                other if other.starts_with('-') => bail!("unknown lint argument: {other}"),
                other => {
                    if target_seen {
                        bail!("vo-dev lint accepts at most one target");
                    }
                    target = other.to_string();
                    target_seen = true;
                }
            }
        }
        Ok(Self { target })
    }
}

fn git_lines(root: &Path, args: &[&str]) -> Result<Vec<String>> {
    let output = Command::new("git")
        .args(args)
        .current_dir(root)
        .output()
        .with_context(|| format!("could not run git {}", args.join(" ")))?;
    if !output.status.success() {
        bail!(
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(str::to_owned)
        .collect())
}

fn lint_repo_boundaries(root: &Path) -> Result<()> {
    let project = load_project(root)?;
    if project.version != 1 {
        bail!("eng/project.toml version must be 1");
    }
    if project.repo.name != "volang" {
        bail!("repo name must be volang");
    }
    if project.repo.module != "github.com/vo-lang/volang" {
        bail!("repo module must be github.com/vo-lang/volang");
    }
    let mut seen = HashSet::new();
    for repo in project
        .first_party
        .iter()
        .chain(project.external_project.iter())
    {
        if !seen.insert(repo.name.clone()) {
            bail!("duplicate project repo: {}", repo.name);
        }
        if repo.local_hint.as_deref().unwrap_or("").trim().is_empty() {
            bail!("project repo {} local_hint cannot be empty", repo.name);
        }
        if project
            .first_party
            .iter()
            .any(|item| item.name == repo.name)
            && repo.repository.as_deref().unwrap_or("").trim().is_empty()
        {
            bail!("first-party repo {} repository cannot be empty", repo.name);
        }
        validate_project_workspaces(root, repo)?;
    }
    lint_repo_boundary_text(root, &project)?;
    Ok(())
}

fn validate_project_workspaces(root: &Path, repo: &ProjectRepo) -> Result<()> {
    let mut seen = HashSet::new();
    for workspace in &repo.workspace {
        validate_ascii_slug("project workspace name", &workspace.name, &['-'])?;
        if !seen.insert(workspace.name.clone()) {
            bail!(
                "project repo {} has duplicate workspace {}",
                repo.name,
                workspace.name
            );
        }
        if !matches!(workspace.kind.as_str(), "node") {
            bail!(
                "project repo {} workspace {} has invalid kind {}",
                repo.name,
                workspace.name,
                workspace.kind
            );
        }
        validate_repo_path_like(
            "project workspace",
            &format!("{}/{}", repo.name, workspace.name),
            "path",
            &workspace.path,
            false,
        )?;
        if let Some(local_hint) = &repo.local_hint {
            let local_root = root.join(local_hint);
            if local_root.exists() && !local_root.join(&workspace.path).is_dir() {
                bail!(
                    "project repo {} workspace {} path is missing under local_hint: {}",
                    repo.name,
                    workspace.name,
                    workspace.path
                );
            }
        }
    }
    Ok(())
}

fn lint_repo_boundary_text(root: &Path, project: &crate::config::ProjectFile) -> Result<()> {
    let mut denied = project
        .first_party
        .iter()
        .chain(project.external_project.iter())
        .filter_map(|repo| repo.local_hint.as_deref())
        .map(str::to_owned)
        .collect::<BTreeSet<_>>();
    denied.extend(
        ["ROOT.parent", "PROJECT_ROOT.parent", "~/.vo/mod"]
            .into_iter()
            .map(str::to_owned),
    );
    let mut violations = Vec::new();
    let mut paths = BTreeSet::new();
    for args in [
        ["ls-files"].as_slice(),
        ["ls-files", "--others", "--exclude-standard"].as_slice(),
    ] {
        for path in git_lines(root, args)? {
            paths.insert(path);
        }
    }
    for path in paths {
        if !is_repo_boundary_automation_file(&path) {
            continue;
        }
        let full = root.join(&path);
        let Ok(text) = fs::read_to_string(&full) else {
            continue;
        };
        for needle in &denied {
            if text.contains(needle) {
                violations.push(format!(
                    "{path} contains direct boundary reference {needle}"
                ));
            }
        }
    }
    if !violations.is_empty() {
        bail!("repo boundary violations: {}", violations.join("; "));
    }
    Ok(())
}

fn is_repo_boundary_automation_file(path: &str) -> bool {
    path == "d.py"
}

fn lint_layout(root: &Path) -> Result<()> {
    for old_path in [
        "studio",
        ".examples",
        "lang/test_data",
        "cmd/vo-test/rust",
        ".vo-cache",
        ".volang/studio",
        "assets",
    ] {
        if root.join(old_path).exists() {
            bail!("old layout path still exists: {old_path}");
        }
    }
    for required in [
        "cmd/vo-test/Cargo.toml",
        "tests/lang/manifest.toml",
        "tests/lang/cases",
        "tests/lang/projects",
        "tests/lang/archives",
        "tests/lang/fixtures",
        "tests/fixtures",
        "examples/manifest.toml",
        "benchmarks/manifest.toml",
        "vo.work",
    ] {
        if !root.join(required).exists() {
            bail!("required layout path is missing: {required}");
        }
    }

    let allowed_root_files = BTreeSet::from([
        ".gitattributes",
        ".gitignore",
        "Cargo.lock",
        "Cargo.toml",
        "LICENSE",
        "README.md",
        "d.py",
        "CHANGELOG.md",
        "CONTRIBUTING.md",
        "GOVERNANCE.md",
        "rust-toolchain.toml",
        "SECURITY.md",
        "vo.work",
    ]);
    for entry in fs::read_dir(root)? {
        let entry = entry?;
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let name = entry.file_name().to_string_lossy().to_string();
        if !allowed_root_files.contains(name.as_str()) {
            bail!("unapproved repository-root file: {name}");
        }
        if name.ends_with(".vo") || name.ends_with(".vob") {
            bail!("root scratch/build output file is not allowed: {name}");
        }
    }
    Ok(())
}

fn lint_docs(root: &Path) -> Result<()> {
    lint_current_markdown(root)?;
    crate::generate_docs::check_studio_docs(root)?;
    lint_touched_dev_note_front_matter(root)?;
    Ok(())
}

fn lint_current_markdown(root: &Path) -> Result<()> {
    let mut files = collect_relative_files(root, &root.join("docs"), "md")?;
    files.extend(collect_relative_files(
        root,
        &root.join("lang/docs/guides"),
        "md",
    )?);
    files.extend(collect_relative_files(root, &root.join("ui/docs"), "md")?);
    files.sort();
    if files.len() < 20 {
        bail!("current repository documentation set is unexpectedly empty");
    }
    for relative in files {
        let source = read_utf8_regular_file_limited(
            &root.join(&relative),
            &format!("current documentation {relative}"),
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
        )?;
        let first = source.lines().find(|line| !line.trim().is_empty());
        if !first.is_some_and(|line| line.starts_with("# ")) {
            bail!("current documentation {relative} must begin with a level-one heading");
        }
    }
    Ok(())
}

fn lint_volang_skill(root: &Path) -> Result<()> {
    const MAX_SKILL_FILE_BYTES: usize = 64 * 1024;
    const MAX_SKILL_LINES: usize = 160;
    let skill_root = root.join("skills/volang-dev");
    let skill_path = skill_root.join("SKILL.md");
    let skill =
        read_utf8_regular_file_limited(&skill_path, "volang-dev skill", MAX_SKILL_FILE_BYTES)?;
    let (front_matter, body) = skill
        .strip_prefix("---\n")
        .and_then(|rest| rest.split_once("\n---\n"))
        .ok_or_else(|| anyhow!("skills/volang-dev/SKILL.md has invalid front matter"))?;
    let mut fields = BTreeMap::new();
    for line in front_matter.lines() {
        let (key, value) = line.split_once(':').ok_or_else(|| {
            anyhow!("volang-dev skill front matter contains invalid line {line:?}")
        })?;
        let key = key.trim();
        let value = value.trim();
        if key.is_empty() || value.is_empty() {
            bail!("volang-dev skill front matter contains an empty key or value");
        }
        if fields.insert(key, value).is_some() {
            bail!("volang-dev skill front matter repeats key {key}");
        }
    }
    if fields.get("name").copied() != Some("volang-dev") {
        bail!("volang-dev skill front matter must declare name: volang-dev");
    }
    if !fields.contains_key("description") || fields.len() != 2 {
        bail!("volang-dev skill front matter must contain exactly name and description");
    }
    if !body.starts_with("\n# Volang Development\n") {
        bail!("volang-dev skill body must begin with # Volang Development");
    }
    let line_count = skill.lines().count();
    if line_count > MAX_SKILL_LINES {
        bail!("volang-dev skill has {line_count} lines; limit is {MAX_SKILL_LINES}");
    }
    let entries = fs::read_dir(&skill_root)
        .map_err(|error| anyhow!("could not read {}: {error}", skill_root.display()))?;
    for entry in entries {
        let entry = entry?;
        if entry.file_name() != "SKILL.md" {
            bail!(
                "volang-dev is a single-file skill; remove {}",
                entry.path().display()
            );
        }
    }
    Ok(())
}

fn lint_touched_dev_note_front_matter(root: &Path) -> Result<()> {
    let mut paths = BTreeSet::new();
    for args in [
        ["diff", "--name-only"].as_slice(),
        ["diff", "--cached", "--name-only"].as_slice(),
        ["ls-files", "--others", "--exclude-standard"].as_slice(),
    ] {
        for path in git_lines(root, args)? {
            if path.starts_with("lang/docs/dev-notes/") && path.ends_with(".md") {
                paths.insert(path);
            }
        }
    }

    for path in paths {
        let abs = root.join(&path);
        if !abs.is_file() {
            continue;
        }
        let text = fs::read_to_string(&abs)
            .map_err(|err| anyhow!("could not read dev note {path}: {err}"))?;
        let Some(rest) = text.strip_prefix("---\n") else {
            bail!("dev note {path} is missing lifecycle front matter");
        };
        let Some((front_matter, _body)) = rest.split_once("\n---\n") else {
            bail!("dev note {path} has unterminated lifecycle front matter");
        };
        for key in [
            "date:",
            "status:",
            "area:",
            "owner:",
            "supersedes:",
            "superseded_by:",
        ] {
            if !front_matter.lines().any(|line| line.starts_with(key)) {
                bail!("dev note {path} front matter is missing {key}");
            }
        }
        let status = front_matter
            .lines()
            .find_map(|line| line.strip_prefix("status:"))
            .map(str::trim)
            .unwrap_or("");
        if !matches!(status, "design" | "implemented" | "superseded" | "archived") {
            bail!("dev note {path} has invalid status {status:?}");
        }
    }
    Ok(())
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ExamplesManifest {
    version: u32,
    #[serde(default, rename = "example")]
    examples: Vec<ExampleEntry>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ExampleEntry {
    id: String,
    path: String,
    kind: String,
    description: String,
    #[serde(default)]
    expected_targets: Vec<String>,
    owner: String,
}

fn read_regular_file_limited(path: &Path, label: &str, limit: usize) -> Result<Vec<u8>> {
    let metadata = fs::symlink_metadata(path)
        .map_err(|error| anyhow!("could not inspect {}: {error}", path.display()))?;
    if metadata.file_type().is_symlink() || !metadata.file_type().is_file() {
        bail!(
            "{label} {} must be a regular non-symlink file",
            path.display()
        );
    }
    if metadata.len() > limit as u64 {
        bail!("{label} {} exceeds the {limit}-byte limit", path.display());
    }
    let file = fs::File::open(path)
        .map_err(|error| anyhow!("could not open {}: {error}", path.display()))?;
    let mut bytes = Vec::with_capacity((metadata.len() as usize).min(limit));
    file.take(limit as u64 + 1)
        .read_to_end(&mut bytes)
        .map_err(|error| anyhow!("could not read {}: {error}", path.display()))?;
    if bytes.len() > limit {
        bail!("{label} {} exceeds the {limit}-byte limit", path.display());
    }
    Ok(bytes)
}

fn read_utf8_regular_file_limited(path: &Path, label: &str, limit: usize) -> Result<String> {
    let bytes = read_regular_file_limited(path, label, limit)?;
    String::from_utf8(bytes)
        .map_err(|error| anyhow!("{label} {} is not UTF-8: {error}", path.display()))
}

fn source_external_imports(source: &str, label: &str) -> Result<BTreeSet<String>> {
    let (file, diagnostics, _) = vo_syntax::parser::parse(source, 0);
    if diagnostics.has_errors() {
        bail!("{label} contains Vo syntax errors");
    }
    let mut external = BTreeSet::new();
    for import in &file.imports {
        let import_path = import.path.value.as_str();
        let class = vo_module::identity::classify_import(import_path)
            .map_err(|error| anyhow!("{label} has invalid import {import_path:?}: {error}"))?;
        if class == vo_module::identity::ImportClass::External {
            external.insert(import_path.to_string());
        }
    }
    Ok(external)
}

fn lint_single_file_source(source: &str, label: &str) -> Result<()> {
    vo_module::inline_mod::parse_inline_mod_from_source(source)
        .map_err(|error| anyhow!("{label} has invalid inline module authority: {error}"))?;
    if let Some(import) = source_external_imports(source, label)?.into_iter().next() {
        bail!(
            "{label} imports external module {import:?}; single-file sources are dependency-free, so move it into a project with vo.mod"
        );
    }
    Ok(())
}

fn find_example_project_root(path: &Path, examples_root: &Path) -> Result<Option<PathBuf>> {
    let Some(mut current) = path.parent() else {
        return Ok(None);
    };
    loop {
        let manifest = current.join("vo.mod");
        match fs::symlink_metadata(&manifest) {
            Ok(metadata) => {
                if metadata.file_type().is_symlink() || !metadata.file_type().is_file() {
                    bail!(
                        "example project manifest {} must be a regular non-symlink file",
                        manifest.display()
                    );
                }
                return Ok(Some(current.to_path_buf()));
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => {
                return Err(anyhow!("could not inspect {}: {error}", manifest.display()));
            }
        }
        if current == examples_root {
            return Ok(None);
        }
        let Some(parent) = current.parent() else {
            return Ok(None);
        };
        current = parent;
    }
}

fn lint_project_example(path: &Path, examples_root: &Path, label: &str) -> Result<()> {
    let project_root = find_example_project_root(path, examples_root)?
        .ok_or_else(|| anyhow!("{label} has no containing vo.mod"))?;
    let manifest_path = project_root.join("vo.mod");
    let manifest_source = read_utf8_regular_file_limited(
        &manifest_path,
        &format!("{label} vo.mod"),
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
    )?;
    let manifest = vo_module::schema::modfile::ModFile::parse_project(&manifest_source)
        .map_err(|error| anyhow!("{label} project authority is invalid: {error}"))?;
    let source = read_utf8_regular_file_limited(path, label, vo_common::vfs::MAX_TEXT_FILE_BYTES)?;
    for import in source_external_imports(&source, label)? {
        let owned = manifest
            .module
            .as_public()
            .is_some_and(|module| module.owns_import(&import).is_some())
            || manifest
                .dependencies
                .iter()
                .any(|dependency| dependency.module.owns_import(&import).is_some());
        if !owned {
            bail!("{label} imports {import:?}, which is outside its vo.mod dependency closure");
        }
    }
    Ok(())
}

fn lint_examples(root: &Path) -> Result<()> {
    if root.join(".examples").exists() {
        bail!(".examples must not exist");
    }
    let manifest_path = root.join("examples/manifest.toml");
    let manifest_text = read_utf8_regular_file_limited(
        &manifest_path,
        "examples manifest",
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
    )?;
    let manifest: ExamplesManifest = toml::from_str(&manifest_text)
        .map_err(|err| anyhow!("could not parse examples manifest: {err}"))?;
    if manifest.version != 1 {
        bail!("examples/manifest.toml version must be 1");
    }
    let examples_root = root.join("examples");
    let mut ids = HashSet::new();
    let mut listed = BTreeSet::new();
    for example in &manifest.examples {
        validate_ascii_slug("example id", &example.id, &['-'])?;
        if !ids.insert(example.id.clone()) {
            bail!("duplicate example id {}", example.id);
        }
        if !matches!(example.kind.as_str(), "file" | "project-file") {
            bail!(
                "example {} has unsupported kind {}",
                example.id,
                example.kind
            );
        }
        if example.description.trim().is_empty() || example.owner.trim().is_empty() {
            bail!("example {} must declare description and owner", example.id);
        }
        if example.expected_targets.is_empty() {
            bail!("example {} must declare expected_targets", example.id);
        }
        validate_repo_path_like("example", &example.id, "path", &example.path, false)?;
        let path = root.join("examples").join(&example.path);
        let source = read_utf8_regular_file_limited(
            &path,
            &format!("example {}", example.id),
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
        )?;
        if example.kind == "project-file" {
            lint_project_example(&path, &examples_root, &format!("example {}", example.id))?;
        } else {
            lint_single_file_source(&source, &format!("example {}", example.id))?;
        }
        listed.insert(example.path.clone());
    }
    let actual = collect_relative_files(root, &root.join("examples"), "vo")?;
    let actual: BTreeSet<_> = actual.into_iter().collect();
    if actual != listed {
        let missing: Vec<_> = actual.difference(&listed).cloned().collect();
        let extra: Vec<_> = listed.difference(&actual).cloned().collect();
        bail!(
            "examples/manifest.toml is not in sync; missing=[{}] extra=[{}]",
            missing.join(", "),
            extra.join(", ")
        );
    }

    Ok(())
}

#[derive(Debug, Deserialize)]
struct BenchmarksManifest {
    version: u32,
    #[serde(default, rename = "benchmark")]
    benchmarks: Vec<BenchmarkEntry>,
}

#[derive(Debug, Deserialize)]
struct BenchmarkEntry {
    id: String,
    path: String,
    owner: String,
    #[serde(default)]
    languages: Vec<String>,
}

fn lint_benchmarks(root: &Path) -> Result<()> {
    let manifest_path = root.join("benchmarks/manifest.toml");
    let manifest_text = fs::read_to_string(&manifest_path)
        .map_err(|err| anyhow!("could not read {}: {err}", manifest_path.display()))?;
    let manifest: BenchmarksManifest = toml::from_str(&manifest_text)
        .map_err(|err| anyhow!("could not parse benchmarks manifest: {err}"))?;
    if manifest.version != 1 {
        bail!("benchmarks/manifest.toml version must be 1");
    }
    let mut ids = HashSet::new();
    let mut listed = BTreeSet::new();
    for benchmark in &manifest.benchmarks {
        validate_ascii_slug("benchmark id", &benchmark.id, &['-'])?;
        if !ids.insert(benchmark.id.clone()) {
            bail!("duplicate benchmark id {}", benchmark.id);
        }
        if benchmark.owner.trim().is_empty() || benchmark.languages.is_empty() {
            bail!(
                "benchmark {} must declare owner and languages",
                benchmark.id
            );
        }
        validate_repo_path_like("benchmark", &benchmark.id, "path", &benchmark.path, false)?;
        let path = root.join("benchmarks").join(&benchmark.path);
        if !path.is_dir() {
            bail!(
                "benchmark {} path is missing: {}",
                benchmark.id,
                benchmark.path
            );
        }
        if !path
            .join(format!("{}.vo", benchmark_file_stem(&benchmark.path)))
            .is_file()
            && first_file_with_extension(&path, "vo")?.is_none()
        {
            bail!("benchmark {} has no .vo source", benchmark.id);
        }
        listed.insert(benchmark.path.clone());
    }
    for entry in fs::read_dir(root.join("benchmarks"))? {
        let entry = entry?;
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let name = entry.file_name().to_string_lossy().to_string();
        if name == "results" {
            continue;
        }
        if !listed.contains(&name) {
            bail!("benchmark directory is not listed in manifest: {name}");
        }
    }
    lint_no_benchmark_build_products(root, &root.join("benchmarks"))?;
    Ok(())
}

fn lint_no_benchmark_build_products(root: &Path, dir: &Path) -> Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            lint_no_benchmark_build_products(root, &path)?;
            continue;
        }
        let name = entry.file_name().to_string_lossy().to_string();
        if name == "go_bench" || name == "c_bench" || name.ends_with(".class") {
            let rel = path
                .strip_prefix(root)
                .unwrap_or(&path)
                .to_string_lossy()
                .replace('\\', "/");
            bail!("benchmark build product must not be committed or left in tree: {rel}");
        }
    }
    Ok(())
}

const MAX_RELATIVE_FILE_SCAN_DEPTH: usize = 32;
const MAX_RELATIVE_FILE_SCAN_ENTRIES: usize = vo_module::MAX_SOURCE_ARCHIVE_ENTRIES;

fn collect_relative_files(root: &Path, dir: &Path, extension: &str) -> Result<Vec<String>> {
    let metadata = fs::symlink_metadata(dir)
        .map_err(|error| anyhow!("could not inspect scan root {}: {error}", dir.display()))?;
    if metadata.file_type().is_symlink() || !metadata.file_type().is_dir() {
        bail!(
            "scan root {} must be a directory without symbolic links",
            dir.display()
        );
    }
    let mut out = Vec::new();
    let mut entries = 0usize;
    collect_relative_files_inner(root, dir, extension, 0, &mut entries, &mut out)?;
    out.sort();
    Ok(out)
}

fn collect_relative_files_inner(
    root: &Path,
    dir: &Path,
    extension: &str,
    depth: usize,
    entries: &mut usize,
    out: &mut Vec<String>,
) -> Result<()> {
    if depth > MAX_RELATIVE_FILE_SCAN_DEPTH {
        bail!(
            "file scan exceeds the {}-directory depth limit at {}",
            MAX_RELATIVE_FILE_SCAN_DEPTH,
            dir.display()
        );
    }
    let directory = fs::read_dir(dir)
        .map_err(|error| anyhow!("could not read directory {}: {error}", dir.display()))?;
    for entry in directory {
        let entry =
            entry.map_err(|error| anyhow!("could not read entry in {}: {error}", dir.display()))?;
        *entries = entries
            .checked_add(1)
            .ok_or_else(|| anyhow!("file scan entry count overflow"))?;
        if *entries > MAX_RELATIVE_FILE_SCAN_ENTRIES {
            bail!(
                "file scan exceeds the {}-entry limit",
                MAX_RELATIVE_FILE_SCAN_ENTRIES
            );
        }
        let name = entry.file_name().into_string().map_err(|_| {
            anyhow!(
                "directory {} contains a non-UTF-8 entry name",
                dir.display()
            )
        })?;
        let path = entry.path();
        let metadata = fs::symlink_metadata(&path)
            .map_err(|error| anyhow!("could not inspect {}: {error}", path.display()))?;
        if metadata.file_type().is_symlink() {
            bail!("file scan rejects symbolic link {}", path.display());
        }
        if metadata.file_type().is_dir() {
            collect_relative_files_inner(root, &path, extension, depth + 1, entries, out)?;
        } else if metadata.file_type().is_file()
            && Path::new(&name)
                .extension()
                .and_then(|value| value.to_str())
                == Some(extension)
        {
            let rel = path
                .strip_prefix(root.join("examples"))
                .or_else(|_| path.strip_prefix(root))
                .map_err(|_| anyhow!("scanned path {} escaped repository root", path.display()))?;
            let rel = rel
                .to_str()
                .ok_or_else(|| anyhow!("scanned path {} is not UTF-8", path.display()))?;
            out.push(rel.replace('\\', "/"));
        } else if !metadata.file_type().is_file() {
            bail!("file scan rejects special entry {}", path.display());
        }
    }
    Ok(())
}

fn first_file_with_extension(dir: &Path, extension: &str) -> Result<Option<PathBuf>> {
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.extension().and_then(|value| value.to_str()) == Some(extension) {
            return Ok(Some(path));
        }
    }
    Ok(None)
}

fn benchmark_file_stem(path: &str) -> String {
    path.rsplit('/').next().unwrap_or(path).replace('-', "_")
}

#[cfg(test)]
mod tests;
