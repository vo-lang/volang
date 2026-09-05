use anyhow::{bail, Context, Result};
use serde_json::json;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::Path;

const INPUTS: &[&str] = &[
    "lang/protocol/app-runtime/app.schema.toml",
    "lang/crates/vo-schema-compiler/src/lib.rs",
    "cmd/vo-dev/src/generate_protocol.rs",
];

pub(crate) fn cmd_generate(root: &Path, args: Vec<String>) -> Result<()> {
    let mode = parse_mode(&args)?;
    match args.first().map(String::as_str) {
        Some("app-protocol") => generate_app_protocol(root, mode),
        Some("studio-docs") => {
            crate::generate_docs::generate_studio_docs(root, matches!(mode, Mode::Write))
        }
        _ => {
            bail!("usage: vo-dev generate app-protocol|studio-docs --check|--write")
        }
    }
}

fn parse_mode(args: &[String]) -> Result<Mode> {
    match args.get(1).map(String::as_str) {
        Some("--check") if args.len() == 2 => Ok(Mode::Check),
        Some("--write") if args.len() == 2 => Ok(Mode::Write),
        _ => {
            bail!("usage: vo-dev generate app-protocol|studio-docs --check|--write")
        }
    }
}

fn generate_app_protocol(root: &Path, mode: Mode) -> Result<()> {
    let schema_text = fs::read_to_string(root.join(INPUTS[0])).context("read App schema")?;
    let schema =
        vo_schema_compiler::compile_app_schema(&schema_text).context("compile App schema")?;
    let outputs = [
        Output::text("app_protocol.ts", schema.render_typescript()),
        Output::text("app_protocol.vo", schema.render_vo()),
        Output {
            name: "golden-envelope.bin",
            bytes: schema.golden_envelope(),
        },
        Output {
            name: "golden-optional-sections.bin",
            bytes: schema.golden_optional_sections(),
        },
        Output::text(
            "golden-optional-sections.vo",
            schema.render_vo_optional_golden_program(),
        ),
    ];
    let provenance = provenance(root, &outputs)?;
    let mut all = outputs.into_iter().collect::<Vec<_>>();
    all.push(Output::text("provenance.json", provenance));
    let directory = root.join("lang/protocol/app-runtime/generated");
    for output in all {
        let path = directory.join(output.name);
        match mode {
            Mode::Write => {
                fs::create_dir_all(&directory)?;
                fs::write(&path, &output.bytes)
                    .with_context(|| format!("write {}", path.display()))?;
            }
            Mode::Check => {
                let existing = fs::read(&path)
                    .with_context(|| format!("generated output missing: {}", path.display()))?;
                if existing != output.bytes {
                    bail!("generated output is stale: {}", path.display());
                }
            }
        }
    }
    println!("vo-dev generate app-protocol {}: ok", mode.label());
    Ok(())
}

fn provenance(root: &Path, outputs: &[Output]) -> Result<String> {
    let source_digests = INPUTS
        .iter()
        .map(|path| {
            let bytes = fs::read(root.join(path))?;
            Ok((
                (*path).to_string(),
                serde_json::Value::String(digest(&bytes)),
            ))
        })
        .collect::<Result<serde_json::Map<String, serde_json::Value>>>()?;
    let outputs = outputs
        .iter()
        .map(|output| json!({ "path": output.name, "digest": digest(&output.bytes), "size": output.bytes.len() }))
        .collect::<Vec<_>>();
    Ok(serde_json::to_string_pretty(&json!({
        "schemaVersion": 2,
        "artifact": "app-protocol.generated",
        "path": "lang/protocol/app-runtime/generated",
        "generator": { "version": 1, "command": ["cargo", "run", "-q", "-p", "vo-dev", "--locked", "--", "generate", "app-protocol", "--write"] },
        "toolchain": { "rust": "workspace-1.94.0" },
        "sourceDigests": source_digests,
        "inputs": INPUTS,
        "outputs": outputs,
    }))? + "\n")
}

fn digest(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

#[derive(Clone, Copy)]
enum Mode {
    Check,
    Write,
}
impl Mode {
    fn label(&self) -> &'static str {
        match self {
            Self::Check => "--check",
            Self::Write => "--write",
        }
    }
}

struct Output {
    name: &'static str,
    bytes: Vec<u8>,
}
impl Output {
    fn text(name: &'static str, value: String) -> Self {
        Self {
            name,
            bytes: value.into_bytes(),
        }
    }
}
