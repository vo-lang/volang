use anyhow::{bail, Context, Result};
use serde_json::json;
use sha2::{Digest, Sha256};
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

const INPUTS: &[&str] = &[
    "lang/protocol/app-runtime/app.schema.toml",
    "lang/crates/vo-schema-compiler/src/lib.rs",
    "cmd/vo-dev/src/generate_protocol.rs",
];

pub(crate) fn cmd_generate(root: &Path, args: Vec<String>) -> Result<()> {
    let mode = parse_mode(&args)?;
    match args.first().map(String::as_str) {
        Some("app-protocol") => generate_app_protocol(root, mode),
        Some("vogui-protocol") => generate_vogui_protocol(root, mode),
        Some("voplay-protocol") => generate_voplay_protocol(root, mode),
        _ => bail!(
            "usage: vo-dev generate app-protocol|vogui-protocol|voplay-protocol --check|--write"
        ),
    }
}

fn parse_mode(args: &[String]) -> Result<Mode> {
    match args.get(1).map(String::as_str) {
        Some("--check") if args.len() == 2 => Ok(Mode::Check),
        Some("--write") if args.len() == 2 => Ok(Mode::Write),
        _ => bail!(
            "usage: vo-dev generate app-protocol|vogui-protocol|voplay-protocol --check|--write"
        ),
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

fn generate_vogui_protocol(root: &Path, mode: Mode) -> Result<()> {
    generate_framework_protocol(
        root,
        mode,
        "vogui",
        "protocol/vogui.schema.toml",
        "vogui.ui",
        "voguiprotocol",
        "vogui_protocol.ts",
        "vogui_protocol.vo",
        "protocol/generated",
        "rust/protocol/src/generated.rs",
        "vogui-protocol",
    )
}

fn generate_voplay_protocol(root: &Path, mode: Mode) -> Result<()> {
    generate_framework_protocol(
        root,
        mode,
        "voplay",
        "protocol/voplay.schema.toml",
        "voplay.engine",
        "voplayprotocol",
        "voplay_protocol.ts",
        "voplay_protocol.vo",
        "protocol/generated",
        "rust/crates/voplay-protocol/src/generated.rs",
        "voplay-protocol",
    )
}

#[allow(clippy::too_many_arguments)]
fn generate_framework_protocol(
    root: &Path,
    mode: Mode,
    repo_name: &str,
    schema_relative: &str,
    schema_id: &str,
    vo_package: &str,
    typescript_name: &'static str,
    vo_name: &'static str,
    generated_relative: &str,
    rust_relative: &str,
    command_name: &str,
) -> Result<()> {
    let project = crate::config::load_project(root)?;
    let repository = project
        .first_party
        .iter()
        .find(|repo| repo.name == repo_name)
        .and_then(|repo| repo.local_hint.as_deref())
        .map(|hint| root.join(hint))
        .with_context(|| format!("eng/project.toml must declare {repo_name} local_hint"))?;
    let schema_path = repository.join(schema_relative);
    let schema_text =
        fs::read_to_string(&schema_path).with_context(|| format!("read {repo_name} schema"))?;
    let schema = vo_schema_compiler::compile_framework_schema(&schema_text, schema_id)
        .with_context(|| format!("compile {repo_name} schema"))?;
    let outputs = [
        Output::text(typescript_name, schema.render_typescript()),
        Output::text(vo_name, schema.render_vo(vo_package)),
    ];
    let rust_output = Output::text("generated.rs", format_rust(schema.render_rust())?);
    let source_digests = serde_json::Map::from_iter([
        (
            String::from(schema_relative),
            serde_json::Value::String(digest(schema_text.as_bytes())),
        ),
        (
            String::from("external:volang/lang/crates/vo-schema-compiler/src/lib.rs"),
            serde_json::Value::String(digest(&fs::read(root.join(INPUTS[1]))?)),
        ),
    ]);
    let output_manifest = outputs
        .iter()
        .map(|output| {
            json!({ "path": output.name, "digest": digest(&output.bytes), "size": output.bytes.len() })
        })
        .chain(std::iter::once(json!({
            "path": rust_relative,
            "digest": digest(&rust_output.bytes),
            "size": rust_output.bytes.len(),
        })))
        .collect::<Vec<_>>();
    let provenance = serde_json::to_string_pretty(&json!({
        "schemaVersion": 2,
        "artifact": format!("{repo_name}-protocol.generated"),
        "path": generated_relative,
        "generator": { "version": 1, "command": ["cargo", "run", "-q", "-p", "vo-dev", "--locked", "--", "generate", command_name, "--write"] },
        "toolchain": { "rust": "workspace-1.94.0" },
        "sourceDigests": source_digests,
        "outputs": output_manifest,
    }))? + "\n";
    let mut all = outputs.into_iter().collect::<Vec<_>>();
    all.push(Output::text("provenance.json", provenance));
    let directory = repository.join(generated_relative);
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
    let rust_path = repository.join(rust_relative);
    match mode {
        Mode::Write => {
            if let Some(parent) = rust_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&rust_path, &rust_output.bytes)
                .with_context(|| format!("write {}", rust_path.display()))?;
        }
        Mode::Check => {
            let existing = fs::read(&rust_path)
                .with_context(|| format!("generated output missing: {}", rust_path.display()))?;
            if existing != rust_output.bytes {
                bail!("generated output is stale: {}", rust_path.display());
            }
        }
    }
    println!("vo-dev generate {command_name} {}: ok", mode.label());
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

fn format_rust(source: String) -> Result<String> {
    let mut child = Command::new("rustfmt")
        .args(["--emit", "stdout", "--edition", "2021"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("could not start rustfmt for generated protocol")?;
    child
        .stdin
        .take()
        .expect("rustfmt stdin was configured")
        .write_all(source.as_bytes())
        .context("could not write generated protocol to rustfmt")?;
    let output = child
        .wait_with_output()
        .context("could not wait for generated protocol rustfmt")?;
    if !output.status.success() {
        bail!(
            "rustfmt rejected generated protocol: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    String::from_utf8(output.stdout).context("rustfmt generated non-UTF-8 output")
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
