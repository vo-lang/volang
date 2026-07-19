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
    "workspace",
    "docs",
    "skill",
    "studio-web",
    "studio-tauri",
    "examples",
    "benchmarks",
    "release",
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
        "workspace" => lint_workspace(root)?,
        "docs" => lint_docs(root)?,
        "skill" => lint_volang_skill(root)?,
        "studio-web" => lint_studio_web(root)?,
        "studio-tauri" => lint_studio_tauri(root)?,
        "examples" => lint_examples(root)?,
        "benchmarks" => lint_benchmarks(root)?,
        "release" => release_system::lint_release(root)?,
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
        "apps/studio",
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
        ".gitattributes",
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

fn lint_workspace(root: &Path) -> Result<()> {
    let path = root.join("vo.work");
    let source = read_utf8_regular_file_limited(
        &path,
        "root workspace manifest",
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
    )?;
    let work = vo_module::schema::workfile::WorkFile::parse(&source)
        .map_err(|error| anyhow!("root vo.work is invalid: {error}"))?;
    let canonical = work
        .render()
        .map_err(|error| anyhow!("root vo.work cannot be rendered canonically: {error}"))?;
    if source != canonical {
        bail!("root vo.work must use the canonical version-1 representation");
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
    lint_current_markdown(root)?;
    for old_path in ["apps/studio/docs/_manifest.json"] {
        if root.join(old_path).exists() {
            bail!("old docs path still exists: {old_path}");
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
    lint_jit_runtime_path_wording(root)?;
    lint_touched_dev_note_front_matter(root)?;
    Ok(())
}

fn lint_current_markdown(root: &Path) -> Result<()> {
    let mut files = collect_relative_files(root, &root.join("docs"), "md")?;
    files.sort();
    if files.len() < 2 {
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

fn lint_studio_web(root: &Path) -> Result<()> {
    const MAX_WEB_METADATA_BYTES: usize = 512 * 1024;
    let studio = root.join("apps/studio");
    let cname = read_utf8_regular_file_limited(
        &studio.join("public/CNAME"),
        "Studio site domain",
        MAX_WEB_METADATA_BYTES,
    )?;
    if cname != "volang.dev\n" {
        bail!("Studio public/CNAME must contain exactly volang.dev followed by a newline");
    }
    let status = Command::new("node")
        .args(["--check", "scripts/build_wasm.mjs"])
        .current_dir(&studio)
        .status()
        .map_err(|error| anyhow!("could not syntax-check Studio WASM builder: {error}"))?;
    if !status.success() {
        bail!("Studio WASM builder contains invalid JavaScript");
    }
    Ok(())
}

fn lint_studio_tauri(root: &Path) -> Result<()> {
    const MAX_DESKTOP_METADATA_BYTES: usize = 2 * 1024 * 1024;
    let tauri_root = root.join("apps/studio/src-tauri");

    let cargo_manifest_path = tauri_root.join("Cargo.toml");
    let cargo_manifest = read_utf8_regular_file_limited(
        &cargo_manifest_path,
        "Studio Tauri Cargo manifest",
        MAX_DESKTOP_METADATA_BYTES,
    )?;
    let cargo: toml::Value = toml::from_str(&cargo_manifest)
        .map_err(|error| anyhow!("Studio Tauri Cargo.toml is invalid: {error}"))?;
    let package = cargo
        .get("package")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| anyhow!("Studio Tauri Cargo.toml is missing [package]"))?;
    let cargo_version = package
        .get("version")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| anyhow!("Studio Tauri Cargo.toml is missing package.version"))?;
    let cargo_edition = package
        .get("edition")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| anyhow!("Studio Tauri Cargo.toml is missing package.edition"))?;
    if package.get("name").and_then(toml::Value::as_str) != Some("studio")
        || package.get("publish").and_then(toml::Value::as_bool) != Some(false)
        || package.get("rust-version").and_then(toml::Value::as_str) != Some("1.94.0")
    {
        bail!("Studio Tauri Cargo.toml must describe the non-publishable studio package");
    }
    let vogui_rev = lint_vogui_protocol_manifest(&cargo)?;
    let vogui_lock_source =
        format!("git+https://github.com/vo-lang/vogui?rev={vogui_rev}#{vogui_rev}");
    let cargo_lock = read_utf8_regular_file_limited(
        &tauri_root.join("Cargo.lock"),
        "Studio Tauri Cargo lockfile",
        MAX_DESKTOP_METADATA_BYTES,
    )?;
    let lock: toml::Value = toml::from_str(&cargo_lock)
        .map_err(|error| anyhow!("Studio Tauri Cargo.lock is invalid: {error}"))?;
    lint_vogui_protocol_lock(&lock, &vogui_lock_source)?;

    let npm_package = read_json_object_limited(
        &root.join("apps/studio/package.json"),
        "Studio package manifest",
        MAX_DESKTOP_METADATA_BYTES,
    )?;
    if npm_package
        .get("version")
        .and_then(serde_json::Value::as_str)
        != Some(cargo_version)
        || npm_package
            .pointer("/scripts/dev")
            .and_then(serde_json::Value::as_str)
            != Some("vite")
        || npm_package
            .pointer("/scripts/build")
            .and_then(serde_json::Value::as_str)
            != Some("vite build")
    {
        bail!("Studio package.json must match the desktop version and canonical Vite scripts");
    }

    let config_path = tauri_root.join("tauri.conf.json");
    let config = read_json_object_limited(
        &config_path,
        "Studio Tauri configuration",
        MAX_DESKTOP_METADATA_BYTES,
    )?;
    if config.get("$schema").and_then(serde_json::Value::as_str)
        != Some("https://schema.tauri.app/config/2")
        || config
            .get("productName")
            .and_then(serde_json::Value::as_str)
            != Some("Studio")
        || config.get("version").and_then(serde_json::Value::as_str) != Some(cargo_version)
        || config.get("identifier").and_then(serde_json::Value::as_str) != Some("com.volang.studio")
        || config
            .pointer("/build/frontendDist")
            .and_then(serde_json::Value::as_str)
            != Some("../dist")
        || config
            .pointer("/build/beforeDevCommand")
            .and_then(serde_json::Value::as_str)
            != Some("npm run dev")
        || config
            .pointer("/build/beforeBuildCommand")
            .and_then(serde_json::Value::as_str)
            != Some("npm run build")
        || config
            .pointer("/build/devUrl")
            .and_then(serde_json::Value::as_str)
            != Some("http://127.0.0.1:5174")
    {
        bail!("Studio Tauri configuration is not bound to the canonical Studio web build");
    }
    let windows = config
        .pointer("/app/windows")
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| anyhow!("Studio Tauri configuration must declare app.windows"))?;
    let mut window_labels = BTreeSet::new();
    for window in windows {
        let label = window
            .get("label")
            .and_then(serde_json::Value::as_str)
            .filter(|label| !label.is_empty())
            .ok_or_else(|| anyhow!("Studio Tauri window is missing a non-empty label"))?;
        if !window_labels.insert(label.to_string()) {
            bail!("Studio Tauri configuration repeats window label {label}");
        }
    }
    if !window_labels.contains("main") {
        bail!("Studio Tauri configuration must declare the main window");
    }

    let capability_path = tauri_root.join("capabilities/default.json");
    let capability = read_json_object_limited(
        &capability_path,
        "Studio Tauri default capability",
        MAX_DESKTOP_METADATA_BYTES,
    )?;
    if capability
        .get("identifier")
        .and_then(serde_json::Value::as_str)
        != Some("default")
    {
        bail!("Studio Tauri default capability must use identifier default");
    }
    let capability_windows = capability
        .get("windows")
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| anyhow!("Studio Tauri default capability is missing windows"))?;
    if !capability_windows.iter().all(|window| {
        window
            .as_str()
            .is_some_and(|label| window_labels.contains(label))
    }) || !capability_windows
        .iter()
        .any(|window| window.as_str() == Some("main"))
    {
        bail!("Studio Tauri default capability references an unknown or missing main window");
    }
    if !capability
        .get("permissions")
        .and_then(serde_json::Value::as_array)
        .is_some_and(|permissions| {
            permissions
                .iter()
                .any(|permission| permission.as_str() == Some("core:default"))
        })
    {
        bail!("Studio Tauri default capability must grant core:default");
    }
    let remote_urls = capability
        .pointer("/remote/urls")
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| anyhow!("Studio Tauri default capability is missing remote.urls"))?
        .iter()
        .map(|value| {
            value
                .as_str()
                .map(str::to_string)
                .ok_or_else(|| anyhow!("Studio Tauri remote URL must be a string"))
        })
        .collect::<Result<BTreeSet<_>>>()?;
    let expected_remote_urls = BTreeSet::from(["http://127.0.0.1:5174/*".to_string()]);
    if remote_urls != expected_remote_urls {
        bail!("Studio Tauri default capability has unexpected development origins");
    }

    let schema_files = collect_relative_files(root, &tauri_root.join("gen/schemas"), "json")?;
    let expected_schema_files = [
        "apps/studio/src-tauri/gen/schemas/acl-manifests.json",
        "apps/studio/src-tauri/gen/schemas/capabilities.json",
        "apps/studio/src-tauri/gen/schemas/desktop-schema.json",
        "apps/studio/src-tauri/gen/schemas/macOS-schema.json",
    ];
    if schema_files
        != expected_schema_files
            .iter()
            .map(|path| path.to_string())
            .collect::<Vec<_>>()
    {
        bail!("Studio Tauri generated schema set is incomplete or contains unexpected files");
    }
    for relative in schema_files {
        read_json_object_limited(
            &root.join(&relative),
            &format!("Studio Tauri generated schema {relative}"),
            MAX_DESKTOP_METADATA_BYTES,
        )?;
    }
    let icon = read_regular_file_limited(
        &tauri_root.join("icons/icon.png"),
        "Studio Tauri tracked icon placeholder",
        MAX_DESKTOP_METADATA_BYTES,
    )?;
    if !icon.starts_with(b"\x89PNG\r\n\x1a\n") {
        bail!("Studio Tauri tracked icon placeholder is not a PNG file");
    }
    let mut rust_sources = collect_relative_files(root, &tauri_root.join("src"), "rs")?;
    rust_sources.push("apps/studio/src-tauri/build.rs".to_string());
    rust_sources.sort();
    let status = Command::new("rustfmt")
        .args(["--edition", cargo_edition, "--check"])
        .args(&rust_sources)
        .current_dir(root)
        .status()
        .map_err(|error| anyhow!("could not execute rustfmt for Studio Tauri: {error}"))?;
    if !status.success() {
        bail!("Studio Tauri Rust sources are not formatted");
    }
    Ok(())
}

fn lint_vogui_protocol_manifest(cargo: &toml::Value) -> Result<String> {
    let dependency = cargo
        .get("dependencies")
        .and_then(toml::Value::as_table)
        .and_then(|dependencies| dependencies.get("vogui-protocol"))
        .and_then(toml::Value::as_table)
        .ok_or_else(|| anyhow!("Studio Tauri must declare vogui-protocol as a table dependency"))?;
    let revision = dependency
        .get("rev")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| {
            anyhow!("Studio Tauri vogui-protocol dependency must declare an exact revision")
        })?;
    if dependency.len() != 2
        || dependency.get("git").and_then(toml::Value::as_str)
            != Some("https://github.com/vo-lang/vogui")
        || revision.len() != 40
        || !revision
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f'))
    {
        bail!("Studio Tauri vogui-protocol dependency must use the canonical Git URL and exact revision");
    }
    Ok(revision.to_string())
}

fn lint_vogui_protocol_lock(lock: &toml::Value, expected_source: &str) -> Result<()> {
    if lock
        .get("version")
        .and_then(toml::Value::as_integer)
        .is_none()
    {
        bail!("Studio Tauri Cargo.lock is missing version");
    }
    let packages = lock
        .get("package")
        .and_then(toml::Value::as_array)
        .ok_or_else(|| anyhow!("Studio Tauri Cargo.lock is missing package records"))?;
    let matching = packages
        .iter()
        .filter(|package| {
            package.get("name").and_then(toml::Value::as_str) == Some("vogui-protocol")
        })
        .collect::<Vec<_>>();
    if matching.len() != 1
        || matching[0].get("version").and_then(toml::Value::as_str) != Some("0.1.0")
        || matching[0].get("source").and_then(toml::Value::as_str) != Some(expected_source)
    {
        bail!("Studio Tauri Cargo.lock must pin exactly one vogui-protocol 0.1.0 package at the canonical Git revision");
    }
    Ok(())
}

fn read_json_object_limited(path: &Path, label: &str, limit: usize) -> Result<serde_json::Value> {
    let source = read_utf8_regular_file_limited(path, label, limit)?;
    let value: serde_json::Value = serde_json::from_str(&source)
        .map_err(|error| anyhow!("{label} {} is invalid JSON: {error}", path.display()))?;
    if !value.is_object() {
        bail!("{label} {} must contain a JSON object", path.display());
    }
    Ok(value)
}

fn lint_jit_runtime_path_wording(root: &Path) -> Result<()> {
    let path = root.join("apps/studio/docs/pages/advanced/backends.md");
    let source = fs::read_to_string(&path)
        .map_err(|err| anyhow!("could not read {}: {err}", path.display()))?;
    for forbidden in ["Graceful fallback", "VM fallback behavior"] {
        if source.contains(forbidden) {
            bail!("Studio backend docs must avoid broad JIT fallback wording: {forbidden}");
        }
    }
    if !source.contains("Invalid strict-JIT metadata")
        || !source.contains("fail fast")
        || !source.contains("VM-managed runtime paths")
    {
        bail!("Studio backend docs must spell out strict-JIT fail-fast runtime-path policy");
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
            .as_github()
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

    for relative in
        collect_relative_files(root, &root.join("apps/studio/src/assets/examples"), "vo")?
    {
        let path = root.join(&relative);
        let source = read_utf8_regular_file_limited(
            &path,
            &format!("Studio example {relative}"),
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
        )?;
        lint_single_file_source(&source, &format!("Studio example {relative}"))?;
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
