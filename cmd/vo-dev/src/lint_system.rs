use crate::artifact_lint::lint_artifacts;
use crate::artifact_repo_lint::path_matches_artifact;
use crate::command_lint::{
    inferred_tools_for_command, validate_embedded_test_task_tools,
    validate_first_party_run_node_workspace,
};
use crate::config::{
    load_artifacts, load_ci, load_project, load_tasks, load_toolchains, ProjectRepo, Task, TaskFile,
};
use crate::lint_policy::{
    contains_glob_meta, declared_repo_names, validate_ascii_slug, validate_repo_path_like,
    validate_structured_input_reference, validate_unique_values,
};
use crate::release_system;
use crate::task_graph::task_map;
use crate::task_planner::git_lines;
use crate::tool_lint::lint_toolchain_file;
use anyhow::{anyhow, bail, Result};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) fn cmd_lint(root: &Path, args: Vec<String>) -> Result<()> {
    let target = args.first().map(String::as_str).unwrap_or("all");
    if args.len() > 1 {
        bail!("vo-dev lint accepts at most one target");
    }
    match target {
        "tasks" => {
            lint_tasks(root)?;
            println!("vo-dev lint tasks: ok");
        }
        "artifacts" => {
            lint_artifacts(root)?;
            println!("vo-dev lint artifacts: ok");
        }
        "repo-boundaries" => {
            lint_repo_boundaries(root)?;
            println!("vo-dev lint repo-boundaries: ok");
        }
        "layout" => {
            lint_layout(root)?;
            println!("vo-dev lint layout: ok");
        }
        "docs" => {
            lint_docs(root)?;
            println!("vo-dev lint docs: ok");
        }
        "examples" => {
            lint_examples(root)?;
            println!("vo-dev lint examples: ok");
        }
        "benchmarks" => {
            lint_benchmarks(root)?;
            println!("vo-dev lint benchmarks: ok");
        }
        "release" => {
            release_system::lint_release(root)?;
            println!("vo-dev lint release: ok");
        }
        "all" => {
            lint_tasks(root)?;
            lint_artifacts(root)?;
            lint_repo_boundaries(root)?;
            lint_layout(root)?;
            lint_docs(root)?;
            lint_examples(root)?;
            lint_benchmarks(root)?;
            release_system::lint_release(root)?;
            println!("vo-dev lint all: ok");
        }
        other => bail!("unknown lint target: {other}"),
    }
    Ok(())
}

fn lint_tasks(root: &Path) -> Result<()> {
    let config = load_tasks(root)?;
    lint_task_file(root, &config)
}

pub(crate) fn lint_task_file(root: &Path, config: &TaskFile) -> Result<()> {
    if config.version != 1 {
        bail!("eng/tasks.toml version must be 1");
    }
    let tools = load_toolchains(root)?;
    lint_toolchain_file(root, &tools)?;
    let project = load_project(root)?;
    let artifacts = load_artifacts(root)?;
    let task_map = task_map(config)?;
    let repo_names = declared_repo_names(&project);
    let node_workspace_names: BTreeSet<_> = tools
        .node_workspace
        .iter()
        .map(|workspace| workspace.name.clone())
        .collect();
    let allowed_tiers = [
        "fast", "test", "contract", "stress", "site", "release", "manual", "legacy",
    ];

    for task in &config.tasks {
        if task.name.trim().is_empty() {
            bail!("task name cannot be empty");
        }
        validate_ascii_slug("task name", &task.name, &['-'])?;
        if task.title.trim().is_empty() {
            bail!("task {} title cannot be empty", task.name);
        }
        if task.command.is_empty() {
            bail!("task {} command cannot be empty", task.name);
        }
        if task.command.iter().any(|arg| arg.trim().is_empty()) {
            bail!("task {} command contains an empty argument", task.name);
        }
        for tool in inferred_tools_for_command(&task.command) {
            if !task.tools.iter().any(|declared| declared == tool) {
                bail!(
                    "task {} command uses {} but tools does not declare {}",
                    task.name,
                    task.command[0],
                    tool
                );
            }
        }
        validate_embedded_test_task_tools(root, task)?;
        validate_unique_values("task", &task.name, "tool", &task.tools)?;
        validate_unique_values("task", &task.name, "node workspace", &task.node_workspaces)?;
        validate_unique_values("task", &task.name, "input", &task.inputs)?;
        validate_unique_values("task", &task.name, "output", &task.outputs)?;
        validate_unique_values("task", &task.name, "dependency", &task.needs)?;
        validate_unique_values("task", &task.name, "platform", &task.platforms)?;
        validate_unique_values("task", &task.name, "tag", &task.tags)?;
        for tag in &task.tags {
            validate_ascii_slug("task tag", tag, &['-', '_', '.'])?;
        }
        if let Some(owner) = &task.owner {
            validate_ascii_slug("task owner", owner, &['-', '_', '.'])?;
        }
        if task.inputs.is_empty() {
            bail!("task {} inputs cannot be empty", task.name);
        }
        if let Some(cwd) = &task.cwd {
            validate_repo_path_like("task", &task.name, "cwd", cwd, false)?;
        }
        for input in &task.inputs {
            validate_repo_path_like("task", &task.name, "input", input, true)?;
            validate_structured_input_reference("task", &task.name, input, &project)?;
        }
        for output in &task.outputs {
            validate_repo_path_like("task", &task.name, "output", output, false)?;
            if contains_glob_meta(output) {
                bail!(
                    "task {} output {} must be a concrete path, not a glob",
                    task.name,
                    output
                );
            }
            if !artifacts
                .artifacts
                .iter()
                .any(|artifact| path_matches_artifact(output, artifact))
            {
                bail!(
                    "task {} output {} is not declared in eng/artifacts.toml",
                    task.name,
                    output
                );
            }
        }
        for key in task.env.keys() {
            if key.trim().is_empty() || key.trim() != key {
                bail!("task {} env key cannot be empty or padded", task.name);
            }
        }
        if task.timeout_sec == Some(0) {
            bail!("task {} timeout_sec must be > 0", task.name);
        }
        for platform in &task.platforms {
            if !matches!(platform.as_str(), "linux" | "macos" | "windows") {
                bail!("task {} has invalid platform {}", task.name, platform);
            }
        }
        if !task.platforms.is_empty() {
            bail!(
                "task {} platforms is reserved but not implemented by the vo-dev runner",
                task.name
            );
        }
        if !allowed_tiers.contains(&task.tier.as_str()) {
            bail!("task {} has invalid tier {}", task.name, task.tier);
        }
        if task.shell {
            bail!(
                "task {} uses shell=true; shell tasks are not allowed in the first implementation",
                task.name
            );
        }
        for tool in &task.tools {
            if !tools.tools.contains_key(tool) {
                bail!("task {} references undeclared tool {}", task.name, tool);
            }
        }
        if task.tools.iter().any(|tool| tool == "node") && task.node_workspaces.is_empty() {
            bail!(
                "task {} declares node but does not declare node_workspaces",
                task.name
            );
        }
        for workspace in &task.node_workspaces {
            if !node_workspace_names.contains(workspace) {
                bail!(
                    "task {} references undeclared node workspace {}",
                    task.name,
                    workspace
                );
            }
        }
        validate_first_party_run_node_workspace(task, &tools.node_workspace, &project)?;
        if let Some(repo) = &task.repo {
            if !repo_names.contains(repo) {
                bail!("task {} references undeclared repo {}", task.name, repo);
            }
        }
        for dep in &task.needs {
            if !task_map.contains_key(dep) {
                bail!("task {} depends on unknown task {}", task.name, dep);
            }
        }
    }
    for (group, items) in &config.groups {
        validate_ascii_slug("group name", group, &['-'])?;
        validate_unique_values("group", group, "item", items)?;
        if items.is_empty() {
            bail!("group {group} cannot be empty");
        }
        for item in items {
            if !task_map.contains_key(item) && !config.groups.contains_key(item) {
                bail!("group {group} references unknown task or group {item}");
            }
        }
    }
    for task in task_map.keys() {
        detect_task_cycle(task, &task_map, &mut Vec::new(), &mut HashSet::new())?;
    }
    for group in config.groups.keys() {
        detect_group_cycle(group, config, &mut Vec::new(), &mut HashSet::new())?;
    }
    lint_ci_file(root, &task_map)?;
    Ok(())
}

fn lint_ci_file(root: &Path, task_map: &BTreeMap<String, Task>) -> Result<()> {
    let ci = load_ci(root)?;
    if ci.version != 1 {
        bail!("eng/ci.toml version must be 1");
    }
    match ci
        .changed_files
        .unknown_path_policy
        .as_deref()
        .unwrap_or("fallback")
    {
        "fallback" => {
            if ci.changed_files.fallback.is_empty() {
                bail!("eng/ci.toml fallback policy requires fallback tasks");
            }
        }
        "error" => {}
        other => bail!("eng/ci.toml has invalid unknown_path_policy {other}"),
    }
    for task in &ci.changed_files.fallback {
        validate_ci_route_task(task_map, "fallback", task)?;
    }
    for prefix in &ci.known_prefix {
        validate_repo_path_like("ci known_prefix", &prefix.path, "path", &prefix.path, false)?;
        if prefix.tasks.is_empty() {
            bail!("eng/ci.toml known_prefix {} has no tasks", prefix.path);
        }
        for task in &prefix.tasks {
            validate_ci_route_task(task_map, &format!("known_prefix {}", prefix.path), task)?;
        }
    }
    Ok(())
}

fn validate_ci_route_task(
    task_map: &BTreeMap<String, Task>,
    owner: &str,
    task: &str,
) -> Result<()> {
    let Some(entry) = task_map.get(task) else {
        bail!("eng/ci.toml {owner} references unknown task {task}");
    };
    if entry.internal {
        bail!("eng/ci.toml {owner} references internal task {task}");
    }
    Ok(())
}

fn detect_task_cycle(
    name: &str,
    task_map: &BTreeMap<String, Task>,
    stack: &mut Vec<String>,
    done: &mut HashSet<String>,
) -> Result<()> {
    if done.contains(name) {
        return Ok(());
    }
    if stack.iter().any(|item| item == name) {
        stack.push(name.to_string());
        bail!("task dependency cycle: {}", stack.join(" -> "));
    }
    stack.push(name.to_string());
    let task = task_map
        .get(name)
        .ok_or_else(|| anyhow!("unknown task {name}"))?;
    for dep in &task.needs {
        detect_task_cycle(dep, task_map, stack, done)?;
    }
    stack.pop();
    done.insert(name.to_string());
    Ok(())
}

fn detect_group_cycle(
    name: &str,
    config: &TaskFile,
    stack: &mut Vec<String>,
    done: &mut HashSet<String>,
) -> Result<()> {
    if done.contains(name) {
        return Ok(());
    }
    if stack.iter().any(|item| item == name) {
        stack.push(name.to_string());
        bail!("group cycle: {}", stack.join(" -> "));
    }
    stack.push(name.to_string());
    if let Some(items) = config.groups.get(name) {
        for item in items {
            if config.groups.contains_key(item) {
                detect_group_cycle(item, config, stack, done)?;
            }
        }
    }
    stack.pop();
    done.insert(name.to_string());
    Ok(())
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
    let tasks = load_tasks(root)?;
    let task_map = task_map(&tasks)?;
    let repo_names = declared_repo_names(&project);
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
        {
            if repo.repository.as_deref().unwrap_or("").trim().is_empty() {
                bail!("first-party repo {} repository cannot be empty", repo.name);
            }
            if repo.ci_checkout.is_none() {
                bail!(
                    "first-party repo {} ci_checkout must be explicit",
                    repo.name
                );
            }
        }
        validate_project_workspaces(root, repo)?;
    }
    for task in task_map.values() {
        if let Some(repo) = &task.repo {
            if !repo_names.contains(repo) {
                bail!("task {} references undeclared repo {}", task.name, repo);
            }
        }
    }
    lint_repo_boundary_text(root)?;
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

fn lint_repo_boundary_text(root: &Path) -> Result<()> {
    let denied = [
        "../vogui",
        "../voplay",
        "../vopack",
        "../vostore",
        "../BlockKart",
        "ROOT.parent",
        "PROJECT_ROOT.parent",
        "~/.vo/mod",
    ];
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
        if !is_repo_boundary_operational_file(&path) {
            continue;
        }
        let full = root.join(&path);
        let Ok(text) = fs::read_to_string(&full) else {
            continue;
        };
        for needle in denied {
            if text.contains(needle) && !is_allowed_repo_boundary_reference(&path) {
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

fn is_repo_boundary_operational_file(path: &str) -> bool {
    if path.starts_with("apps/playground-legacy/src/assets/docs/generated/") {
        return false;
    }
    path == "d.py"
        || path == "vo.work"
        || path.starts_with("scripts/ci/")
        || path.starts_with(".github/")
        || path == "apps/playground-legacy/vite.config.ts"
        || path == "apps/playground-legacy/rust/build.rs"
        || path.starts_with("apps/playground-legacy/src/")
        || path == "apps/studio/src-tauri/Cargo.toml"
        || path.starts_with("apps/studio/src-tauri/src/")
}

fn is_allowed_repo_boundary_reference(path: &str) -> bool {
    matches!(
        path,
        "vo.work"
            | "apps/playground-legacy/vite.config.ts"
            | "apps/playground-legacy/rust/build.rs"
            | "apps/playground-legacy/src/pages/Playground.svelte"
            | "apps/playground-legacy/src/components/GuiPreview.svelte"
            | "apps/studio/src-tauri/Cargo.toml"
            | "apps/studio/src-tauri/src/commands/pathing.rs"
    )
}

fn lint_layout(root: &Path) -> Result<()> {
    for old_path in [
        "studio",
        "playground",
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
        "apps/studio",
        "apps/playground-legacy",
        "cmd/vo-test/Cargo.toml",
        "tests/lang/manifest.toml",
        "tests/lang/cases",
        "tests/lang/projects",
        "tests/lang/archives",
        "tests/lang/fixtures",
        "tests/fixtures",
        "examples/manifest.toml",
        "benchmarks/manifest.toml",
    ] {
        if !root.join(required).exists() {
            bail!("required layout path is missing: {required}");
        }
    }

    let allowed_root_files = BTreeSet::from([
        ".gitignore",
        "Cargo.lock",
        "Cargo.toml",
        "LICENSE",
        "README.md",
        "d.py",
        "rust-toolchain.toml",
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

#[derive(Debug, Deserialize)]
struct StudioDocsManifest {
    version: u32,
    #[serde(default, rename = "section")]
    sections: Vec<StudioDocsSection>,
}

#[derive(Debug, Deserialize)]
struct StudioDocsSection {
    title: String,
    slug: String,
    #[serde(default, rename = "page")]
    pages: Vec<StudioDocsPage>,
}

#[derive(Debug, Deserialize)]
struct StudioDocsPage {
    title: String,
    file: String,
}

fn lint_docs(root: &Path) -> Result<()> {
    for old_path in [
        "apps/playground-legacy/src/assets/docs/spec",
        "apps/playground-legacy/src/assets/docs/vo-for-gophers.md",
        "apps/studio/docs/_manifest.json",
    ] {
        if root.join(old_path).exists() {
            bail!("old docs path still exists: {old_path}");
        }
    }
    let generated = root.join("apps/playground-legacy/src/assets/docs/generated");
    if !generated.join("_manifest.json").is_file() {
        bail!("generated Playground docs manifest is missing");
    }
    for required in [
        "vo-for-gophers.md",
        "spec/language.md",
        "spec/module.md",
        "spec/native-ffi.md",
    ] {
        let path = generated.join(required);
        let text = fs::read_to_string(&path)
            .map_err(|err| anyhow!("could not read generated doc {}: {err}", required))?;
        if !text.starts_with("<!--\nGenerated from ") || !text.contains("\nSource-Digest: sha256:")
        {
            bail!("generated doc {required} is missing provenance header");
        }
    }

    let manifest_path = root.join("apps/studio/docs/manifest.toml");
    let manifest_text = fs::read_to_string(&manifest_path)
        .map_err(|err| anyhow!("could not read {}: {err}", manifest_path.display()))?;
    let manifest: StudioDocsManifest = toml::from_str(&manifest_text)
        .map_err(|err| anyhow!("could not parse Studio docs manifest: {err}"))?;
    if manifest.version != 1 {
        bail!("apps/studio/docs/manifest.toml version must be 1");
    }
    if manifest.sections.is_empty() {
        bail!("Studio docs manifest has no sections");
    }
    let mut pages = HashSet::new();
    for section in &manifest.sections {
        if section.title.trim().is_empty() || section.slug.trim().is_empty() {
            bail!("Studio docs section is missing title or slug");
        }
        if section.pages.is_empty() {
            bail!("Studio docs section {} has no pages", section.slug);
        }
        for page in &section.pages {
            if page.title.trim().is_empty() || page.file.trim().is_empty() {
                bail!(
                    "Studio docs section {} has an incomplete page",
                    section.slug
                );
            }
            if !pages.insert(page.file.clone()) {
                bail!("duplicate Studio docs page {}", page.file);
            }
            if !root
                .join("apps/studio/docs/pages")
                .join(&page.file)
                .is_file()
            {
                bail!("Studio docs manifest references missing page {}", page.file);
            }
        }
    }
    lint_touched_dev_note_front_matter(root)?;
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
struct ExamplesManifest {
    version: u32,
    #[serde(default, rename = "example")]
    examples: Vec<ExampleEntry>,
}

#[derive(Debug, Deserialize)]
struct ExampleEntry {
    id: String,
    path: String,
    kind: String,
    description: String,
    #[serde(default)]
    expected_targets: Vec<String>,
    owner: String,
}

fn lint_examples(root: &Path) -> Result<()> {
    if root.join(".examples").exists() {
        bail!(".examples must not exist");
    }
    let manifest_path = root.join("examples/manifest.toml");
    let manifest_text = fs::read_to_string(&manifest_path)
        .map_err(|err| anyhow!("could not read {}: {err}", manifest_path.display()))?;
    let manifest: ExamplesManifest = toml::from_str(&manifest_text)
        .map_err(|err| anyhow!("could not parse examples manifest: {err}"))?;
    if manifest.version != 1 {
        bail!("examples/manifest.toml version must be 1");
    }
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
        if !path.is_file() {
            bail!(
                "example {} references missing file {}",
                example.id,
                example.path
            );
        }
        if example.kind == "project-file" {
            let Some(parent) = path.parent() else {
                bail!("example {} has no parent directory", example.id);
            };
            if !parent.join("vo.mod").is_file() {
                bail!(
                    "example {} is project-file but {} has no vo.mod",
                    example.id,
                    parent.display()
                );
            }
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

fn collect_relative_files(root: &Path, dir: &Path, extension: &str) -> Result<Vec<String>> {
    let mut out = Vec::new();
    collect_relative_files_inner(root, dir, extension, &mut out)?;
    out.sort();
    Ok(out)
}

fn collect_relative_files_inner(
    root: &Path,
    dir: &Path,
    extension: &str,
    out: &mut Vec<String>,
) -> Result<()> {
    if !dir.is_dir() {
        return Ok(());
    }
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_relative_files_inner(root, &path, extension, out)?;
        } else if path.extension().and_then(|value| value.to_str()) == Some(extension) {
            let rel = path
                .strip_prefix(root.join("examples"))
                .or_else(|_| path.strip_prefix(root))
                .unwrap_or(&path)
                .to_string_lossy()
                .replace('\\', "/");
            out.push(rel);
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
