use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Component, Path};

/// Acceptance declarations point to executable CI tasks. Passing declarations
/// alone never confers certification; evidence must pass the CI bundle verifier.
#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Declaration {
    schema_version: u32,
    product: String,
    sources: Vec<String>,
    tasks: Vec<String>,
}

pub(crate) const REQUIRED_TASKS: [&str; 5] = [
    "rust-quality",
    "ui-web-rewrite",
    "ui-desktop-rewrite-linux",
    "ui-desktop-rewrite-macos",
    "ui-desktop-rewrite-windows",
];

fn load(root: &Path) -> Result<Declaration> {
    let path = root.join("ui/certification.toml");
    let declaration: Declaration = toml::from_str(&fs::read_to_string(&path)?)
        .with_context(|| format!("invalid {}", path.display()))?;
    if declaration.schema_version != 2 || declaration.product != "Volang UI" {
        bail!("invalid UI acceptance declaration identity");
    }
    let required = REQUIRED_TASKS.into_iter().collect::<BTreeSet<_>>();
    let tasks = declaration
        .tasks
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    if tasks != required || tasks.len() != declaration.tasks.len() {
        bail!("UI acceptance requires Rust, Web and all three desktop tasks exactly once");
    }
    let ci: toml::Value = toml::from_str(&fs::read_to_string(root.join("eng/ci.toml"))?)?;
    let ci_tasks = ci
        .get("task")
        .and_then(toml::Value::as_array)
        .context("CI tasks missing")?;
    for task in &declaration.tasks {
        let entry = ci_tasks
            .iter()
            .find(|entry| entry.get("id").and_then(toml::Value::as_str) == Some(task))
            .with_context(|| format!("UI acceptance task {task} is missing"))?;
        if entry
            .get("commands")
            .and_then(toml::Value::as_array)
            .is_none_or(Vec::is_empty)
        {
            bail!("UI acceptance task {task} has no commands");
        }
    }
    if declaration.sources.is_empty() {
        bail!("UI acceptance sources are empty");
    }
    let mut sources = BTreeSet::new();
    for source in &declaration.sources {
        let relative = Path::new(source);
        if source.is_empty()
            || relative.is_absolute()
            || relative
                .components()
                .any(|part| !matches!(part, Component::Normal(_)))
            || !sources.insert(source)
            || !fs::symlink_metadata(root.join(relative))?
                .file_type()
                .is_file()
        {
            bail!("invalid UI acceptance source {source}");
        }
    }
    Ok(declaration)
}

pub(crate) fn cmd_ui_certify(root: &Path, args: Vec<String>) -> Result<()> {
    let declaration = load(root)?;
    let (json, evidence) = match args.as_slice() {
        [] => (false, None),
        [arg] if arg == "--check" => (false, None),
        [arg] if arg == "--json" => (true, None),
        [flag, path] if flag == "--evidence" => (false, Some(path)),
        _ => bail!("usage: vo-dev ui-certify --check|--json|--evidence <ci-bundle>"),
    };
    let status = if let Some(path) = evidence {
        crate::ci::verify_ui_bundle(root, &root.join(path))?;
        "product-certified"
    } else {
        "declaration-valid"
    };
    if json {
        println!(
            "{}",
            serde_json::json!({"declaration":declaration,"status":status})
        );
    } else {
        println!(
            "Volang UI: {status} ({} required CI tasks)",
            declaration.tasks.len()
        );
    }
    Ok(())
}

pub(crate) fn certification_status(root: &Path) -> Result<&'static str> {
    load(root)?;
    Ok("declaration-valid")
}

pub(crate) fn evidence_sources(root: &Path) -> Result<Vec<String>> {
    let mut sources = load(root)?.sources;
    sources.push("ui/certification.toml".to_string());
    Ok(sources)
}
