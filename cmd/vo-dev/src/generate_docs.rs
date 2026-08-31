use crate::lint_policy::{validate_ascii_slug, validate_repo_path_like};
use anyhow::{bail, Context, Result};
use serde::Deserialize;
use serde_json::json;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

const CATALOG_PATH: &str = "lang/docs/catalog.toml";
const GENERATOR_PATH: &str = "cmd/vo-dev/src/generate_docs.rs";
const STUDIO_WEB_MANIFEST: &str = "apps/studio/ui.web.toml";
const OUTPUT_DIRECTORY: &str = "apps/studio/documentation";
const OUTPUT_PATH: &str = "apps/studio/documentation/catalog.vo";
const PROVENANCE_PATH: &str = "apps/studio/documentation/provenance.json";
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
    catalog_vo: Vec<u8>,
    provenance: Vec<u8>,
}

pub(crate) fn generate_studio_docs(root: &Path, write: bool) -> Result<()> {
    let generated = materialize(root)?;
    if write {
        let directory = root.join(OUTPUT_DIRECTORY);
        fs::create_dir_all(&directory)
            .with_context(|| format!("create {}", directory.display()))?;
        reject_unexpected_outputs(&directory)?;
        fs::write(root.join(OUTPUT_PATH), &generated.catalog_vo)
            .with_context(|| format!("write {OUTPUT_PATH}"))?;
        fs::write(root.join(PROVENANCE_PATH), &generated.provenance)
            .with_context(|| format!("write {PROVENANCE_PATH}"))?;
    } else {
        compare(root, OUTPUT_PATH, &generated.catalog_vo)?;
        compare(root, PROVENANCE_PATH, &generated.provenance)?;
        reject_unexpected_outputs(&root.join(OUTPUT_DIRECTORY))?;
    }
    let mode = if write { "--write" } else { "--check" };
    println!("vo-dev generate studio-docs {mode}: ok");
    Ok(())
}

pub(crate) fn check_studio_docs(root: &Path) -> Result<()> {
    let generated = materialize(root)?;
    compare(root, OUTPUT_PATH, &generated.catalog_vo)?;
    compare(root, PROVENANCE_PATH, &generated.provenance)?;
    reject_unexpected_outputs(&root.join(OUTPUT_DIRECTORY))
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
    validate_studio_routes(root, &catalog)?;
    inputs.push(STUDIO_WEB_MANIFEST.to_string());
    inputs.push(GENERATOR_PATH.to_string());

    let catalog_vo = vo_syntax::format_source(&render_vo(&catalog, &loaded))
        .map_err(anyhow::Error::msg)?
        .into_bytes();
    let provenance = render_provenance(root, &inputs, &catalog_vo)?.into_bytes();
    Ok(GeneratedDocs {
        catalog_vo,
        provenance,
    })
}

fn validate_studio_routes(root: &Path, catalog: &Catalog) -> Result<()> {
    let text = fs::read_to_string(root.join(STUDIO_WEB_MANIFEST))
        .with_context(|| format!("read Studio Web manifest {STUDIO_WEB_MANIFEST}"))?;
    let manifest: toml::Value = toml::from_str(&text)
        .with_context(|| format!("parse Studio Web manifest {STUDIO_WEB_MANIFEST}"))?;
    let routes = manifest
        .get("routes")
        .and_then(toml::Value::as_array)
        .context("apps/studio/ui.web.toml routes must be an array")?;
    let found = routes
        .iter()
        .filter_map(toml::Value::as_str)
        .filter(|route| route.starts_with("/docs/"))
        .map(str::to_owned)
        .collect::<BTreeSet<_>>();
    let expected = catalog
        .section
        .iter()
        .flat_map(|section| section.page.iter())
        .map(|page| format!("/docs/{}", page.id))
        .collect::<BTreeSet<_>>();
    if found != expected {
        let missing = expected.difference(&found).cloned().collect::<Vec<_>>();
        let stale = found.difference(&expected).cloned().collect::<Vec<_>>();
        bail!("Studio documentation routes are stale; missing={missing:?}; stale={stale:?}");
    }
    Ok(())
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
            if !page.file.starts_with("lang/docs/") && !page.file.starts_with("ui/docs/") {
                bail!(
                    "documentation page {} must live under lang/docs or ui/docs",
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

fn render_vo(catalog: &Catalog, pages: &[LoadedPage<'_>]) -> String {
    let mut output = String::from(
        "// Code generated from lang/docs/catalog.toml by vo-dev. DO NOT EDIT.\n\
         // Every Markdown byte remains owned by its SourcePath.\n\
         package documentation\n\n\
         import \"github.com/vo-lang/studio/domain\"\n\n",
    );
    output.push_str(&format!(
        "func Version() string {{ return {} }}\n\n",
        quote(&catalog.version)
    ));
    output.push_str(&format!(
        "func PageCount() int {{ return {} }}\n\n",
        pages.len()
    ));
    output.push_str("func Sections() []domain.DocSection {\n\treturn []domain.DocSection{\n");
    for section in &catalog.section {
        output.push_str(&format!(
            "\t\t{{ID: {}, Title: {}}},\n",
            quote(&section.id),
            quote(&section.title)
        ));
    }
    output.push_str("\t}\n}\n\n");
    output.push_str("func Pages() []domain.DocPage {\n\treturn []domain.DocPage{\n");
    for loaded in pages {
        output.push_str("\t\t{\n");
        output.push_str(&format!("\t\t\tID: {},\n", quote(&loaded.page.id)));
        output.push_str(&format!("\t\t\tTitle: {},\n", quote(&loaded.page.title)));
        output.push_str(&format!(
            "\t\t\tSectionID: {},\n",
            quote(&loaded.section.id)
        ));
        output.push_str(&format!(
            "\t\t\tSection: {},\n",
            quote(&loaded.section.title)
        ));
        output.push_str(&format!(
            "\t\t\tSummary: {},\n",
            quote(&loaded.page.summary)
        ));
        output.push_str(&format!(
            "\t\t\tSourcePath: {},\n",
            quote(&loaded.page.file)
        ));
        output.push_str(&format!("\t\t\tMarkdown: {},\n", quote(&loaded.markdown)));
        output.push_str("\t\t},\n");
    }
    output.push_str("\t}\n}\n");
    output
}

fn render_provenance(root: &Path, inputs: &[String], output: &[u8]) -> Result<String> {
    let mut source_digests = BTreeMap::new();
    for input in inputs {
        let bytes = fs::read(root.join(input))
            .with_context(|| format!("read documentation generator input {input}"))?;
        source_digests.insert(input.clone(), digest(&bytes));
    }
    Ok(serde_json::to_string_pretty(&json!({
        "schemaVersion": 2,
        "artifact": "studio-documentation.generated",
        "path": OUTPUT_DIRECTORY,
        "generator": {
            "version": 1,
            "command": ["cargo", "run", "-q", "-p", "vo-dev", "--locked", "--", "generate", "studio-docs", "--write"]
        },
        "toolchain": { "rust": "workspace-1.94.0" },
        "sourceDigests": source_digests,
        "inputs": inputs,
        "outputs": [{
            "path": "catalog.vo",
            "digest": digest(output),
            "size": output.len(),
        }],
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

fn reject_unexpected_outputs(directory: &Path) -> Result<()> {
    if !directory.is_dir() {
        bail!(
            "generated Studio documentation directory is missing: {}",
            directory.display()
        );
    }
    let allowed = BTreeSet::from(["catalog.vo", "provenance.json"]);
    for entry in fs::read_dir(directory).with_context(|| format!("read {}", directory.display()))? {
        let entry = entry?;
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if !entry.file_type()?.is_file() || !allowed.contains(name.as_ref()) {
            bail!(
                "unexpected generated Studio documentation output: {}",
                entry.path().display()
            );
        }
    }
    Ok(())
}
