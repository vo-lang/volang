use crate::lint_policy::{validate_ascii_slug, validate_repo_path_like};
use anyhow::{bail, Context, Result};
use serde::Deserialize;
use serde_json::json;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

mod document;
mod next;

const CATALOG_PATH: &str = "lang/docs/catalog.toml";
const GENERATOR_PATH: &str = "cmd/vo-dev/src/generate_docs.rs";
const MAX_PAGE_BYTES: usize = 1024 * 1024;
const MAX_TOTAL_SOURCE_BYTES: usize = 4 * 1024 * 1024;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Catalog {
    format: u32,
    version: String,
    #[serde(default)]
    section: Vec<Section>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Section {
    id: String,
    title: String,
    #[serde(default)]
    page: Vec<Page>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Page {
    id: String,
    title: String,
    summary: String,
    file: String,
}

struct LoadedPage<'a> {
    section: &'a Section,
    page: &'a Page,
    markdown: String,
}

struct GeneratedDocs {
    next: BTreeMap<String, Vec<u8>>,
}

pub(crate) fn generate_studio_docs(root: &Path, write: bool) -> Result<()> {
    let generated = materialize(root)?;
    next::synchronize(root, &generated.next, write)?;
    let mode = if write { "--write" } else { "--check" };
    println!("vo-dev generate studio-docs {mode}: ok");
    Ok(())
}

pub(crate) fn check_studio_docs(root: &Path) -> Result<()> {
    let generated = materialize(root)?;
    next::synchronize(root, &generated.next, false)
}

fn materialize(root: &Path) -> Result<GeneratedDocs> {
    let catalog_bytes = fs::read(root.join(CATALOG_PATH))
        .with_context(|| format!("read documentation catalog {CATALOG_PATH}"))?;
    let catalog_text = std::str::from_utf8(&catalog_bytes)
        .with_context(|| format!("documentation catalog {CATALOG_PATH} must be UTF-8"))?;
    let catalog: Catalog = toml::from_str(catalog_text)
        .with_context(|| format!("parse documentation catalog {CATALOG_PATH}"))?;
    validate_catalog(&catalog)?;

    let mut total_bytes = 0usize;
    let mut loaded = Vec::new();
    let mut inputs = vec![CATALOG_PATH.to_string()];
    for section in &catalog.section {
        for page in &section.page {
            let bytes = fs::read(root.join(&page.file))
                .with_context(|| format!("read documentation page {}", page.file))?;
            if bytes.len() > MAX_PAGE_BYTES {
                bail!(
                    "documentation page {} exceeds {} bytes",
                    page.file,
                    MAX_PAGE_BYTES
                );
            }
            total_bytes = total_bytes
                .checked_add(bytes.len())
                .context("documentation source byte count overflow")?;
            if total_bytes > MAX_TOTAL_SOURCE_BYTES {
                bail!(
                    "Studio documentation source exceeds {} bytes",
                    MAX_TOTAL_SOURCE_BYTES
                );
            }
            let markdown = String::from_utf8(bytes)
                .with_context(|| format!("documentation page {} must be UTF-8", page.file))?;
            validate_markdown(page, &markdown)?;
            inputs.push(page.file.clone());
            loaded.push(LoadedPage {
                section,
                page,
                markdown,
            });
        }
    }
    inputs.push(GENERATOR_PATH.to_string());
    inputs.extend(
        [
            "cmd/vo-dev/src/generate_docs/document.rs",
            "cmd/vo-dev/src/generate_docs/next.rs",
            "cmd/vo-dev/Cargo.toml",
            "Cargo.lock",
        ]
        .map(str::to_owned),
    );

    let mut next = next::materialize(&catalog, &loaded)?;
    let next_provenance =
        render_provenance(root, &inputs, &next, next::ARTIFACT, next::DIRECTORY)?.into_bytes();
    next.insert("provenance.json".into(), next_provenance);
    Ok(GeneratedDocs { next })
}

fn validate_catalog(catalog: &Catalog) -> Result<()> {
    if catalog.format != 1 {
        bail!("lang/docs/catalog.toml format must be 1");
    }
    if catalog.version.trim().is_empty() || catalog.version.trim() != catalog.version {
        bail!("documentation catalog version cannot be empty or padded");
    }
    if catalog.section.len() < 4 {
        bail!("documentation catalog must contain at least four sections");
    }
    let mut section_ids = BTreeSet::new();
    let mut page_ids = BTreeSet::new();
    let mut page_files = BTreeSet::new();
    let mut page_count = 0usize;
    let mut language_pages = 0usize;
    for section in &catalog.section {
        validate_ascii_slug("documentation section ID", &section.id, &['-'])?;
        if !section_ids.insert(section.id.as_str()) {
            bail!("duplicate documentation section ID {}", section.id);
        }
        if section.title.trim().is_empty() || section.title.trim() != section.title {
            bail!("documentation section {} has an invalid title", section.id);
        }
        if section.page.is_empty() {
            bail!("documentation section {} contains no pages", section.id);
        }
        for page in &section.page {
            page_count += 1;
            validate_ascii_slug("documentation page ID", &page.id, &['-'])?;
            if !page_ids.insert(page.id.as_str()) {
                bail!("duplicate documentation page ID {}", page.id);
            }
            if !page_files.insert(page.file.as_str()) {
                bail!("duplicate documentation source file {}", page.file);
            }
            if page.title.trim().is_empty() || page.title.trim() != page.title {
                bail!("documentation page {} has an invalid title", page.id);
            }
            if page.summary.trim().len() < 24 || page.summary.trim() != page.summary {
                bail!("documentation page {} has an invalid summary", page.id);
            }
            validate_repo_path_like("documentation page", &page.id, "file", &page.file, false)?;
            if !page.file.ends_with(".md") {
                bail!("documentation page {} must reference Markdown", page.id);
            }
            if !page.file.starts_with("lang/docs/") && !page.file.starts_with("ui/next/") {
                bail!(
                    "documentation page {} must live under lang/docs or ui/next",
                    page.id
                );
            }
            if page.file.starts_with("lang/docs/") {
                language_pages += 1;
            }
        }
    }
    if page_count < 20 {
        bail!("documentation catalog must contain at least 20 maintained pages");
    }
    if language_pages * 2 <= page_count {
        bail!("Volang language and toolchain pages must be the catalog majority");
    }
    Ok(())
}

fn validate_markdown(page: &Page, markdown: &str) -> Result<()> {
    let first = markdown.lines().find(|line| !line.trim().is_empty());
    if !first.is_some_and(|line| line.starts_with("# ")) {
        bail!(
            "documentation page {} ({}) must begin with a level-one heading",
            page.id,
            page.file
        );
    }
    if markdown.trim().len() < 200 {
        bail!(
            "documentation page {} ({}) is too small to be maintained product documentation",
            page.id,
            page.file
        );
    }
    let fences = markdown
        .lines()
        .filter(|line| line.trim_start().starts_with("```"))
        .count();
    if fences % 2 != 0 {
        bail!(
            "documentation page {} ({}) has an unclosed code fence",
            page.id,
            page.file
        );
    }
    Ok(())
}

fn render_provenance(
    root: &Path,
    inputs: &[String],
    outputs: &BTreeMap<String, Vec<u8>>,
    artifact: &str,
    directory: &str,
) -> Result<String> {
    let mut source_digests = BTreeMap::new();
    for input in inputs {
        let bytes = fs::read(root.join(input))
            .with_context(|| format!("read documentation generator input {input}"))?;
        source_digests.insert(input.clone(), digest(&bytes));
    }
    Ok(serde_json::to_string_pretty(&json!({
        "schemaVersion": 2,
        "artifact": artifact,
        "path": directory,
        "generator": {
            "version": 2,
            "command": ["cargo", "run", "-q", "-p", "vo-dev", "--locked", "--", "generate", "studio-docs", "--write"]
        },
        "toolchain": { "rust": "workspace-1.94.0" },
        "sourceDigests": source_digests,
        "inputs": inputs,
        "outputs": outputs.iter().map(|(name, bytes)| json!({
            "path": name, "digest": digest(bytes), "size": bytes.len(),
        })).collect::<Vec<_>>(),
    }))? + "\n")
}

fn quote(value: &str) -> String {
    serde_json::to_string(value).expect("serializing a Rust string cannot fail")
}

fn digest(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn compare(root: &Path, relative: &str, expected: &[u8]) -> Result<()> {
    let found = fs::read(root.join(relative))
        .with_context(|| format!("generated Studio documentation is missing: {relative}"))?;
    if found != expected {
        bail!(
            "generated Studio documentation is stale: {relative}; run `cargo run -q -p vo-dev --locked -- generate studio-docs --write`"
        );
    }
    Ok(())
}
