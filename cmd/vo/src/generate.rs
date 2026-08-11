use std::collections::BTreeSet;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

use serde::Deserialize;
use vo_schema_compiler::{
    generator_cache_key, schema_source_fingerprint, validate_generated_path, GeneratorCacheInput,
    GeneratorIdentity,
};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct GenerateFile {
    format: u32,
    generation: Vec<Generation>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Generation {
    provider: String,
    version: String,
    schema_kind: String,
    executable: Option<String>,
    schema: String,
    outputs: Vec<String>,
    #[serde(default)]
    capabilities: Vec<String>,
}

struct Options {
    project: PathBuf,
    write: bool,
    target: String,
}

struct MaterializedOutput {
    relative_path: PathBuf,
    destination: PathBuf,
}

pub(crate) fn cmd_generate(args: &[OsString]) -> i32 {
    if args.len() == 1 && matches!(args[0].to_str(), Some("-h" | "--help" | "help")) {
        print_usage();
        return 0;
    }
    let options = match Options::parse(args) {
        Ok(options) => options,
        Err(error) => {
            eprintln!("[VO:GENERATE:ARGS] {error}");
            print_usage();
            return 2;
        }
    };
    match generate_project(&options) {
        Ok(outputs) => {
            for output in outputs {
                println!("{}", output.destination.display());
            }
            0
        }
        Err(error) => {
            eprintln!("{error}");
            1
        }
    }
}

fn print_usage() {
    println!("usage: vo generate [path] [--target TRIPLE] [--write]");
}

impl Options {
    fn parse(args: &[OsString]) -> Result<Self, String> {
        let mut project = None;
        let mut write = false;
        let mut target = None;
        let mut index = 0;
        while index < args.len() {
            if args[index] == OsStr::new("--write") {
                write = true;
                index += 1;
                continue;
            }
            if args[index] == OsStr::new("--target") {
                let value = args
                    .get(index + 1)
                    .ok_or_else(|| "--target requires a value".to_string())?;
                target = Some(
                    value
                        .to_str()
                        .ok_or_else(|| "target must be UTF-8".to_string())?
                        .to_string(),
                );
                index += 2;
                continue;
            }
            if args[index].as_encoded_bytes().starts_with(b"-") {
                return Err(format!("unknown option {:?}", args[index]));
            }
            if project.replace(PathBuf::from(&args[index])).is_some() {
                return Err(format!("unexpected argument {:?}", args[index]));
            }
            index += 1;
        }
        Ok(Self {
            project: project.unwrap_or_else(|| PathBuf::from(".")),
            write,
            target: target.unwrap_or_else(host_target),
        })
    }
}

fn generate_project(options: &Options) -> Result<Vec<MaterializedOutput>, String> {
    let project = absolute_project_root(&options.project)?;
    let config_path = project.join("vo.generate.toml");
    let config_text = fs::read_to_string(&config_path).map_err(|error| {
        format!(
            "[VO:GENERATE:CONFIG] failed to read {}: {error}",
            config_path.display()
        )
    })?;
    let config: GenerateFile = toml::from_str(&config_text)
        .map_err(|error| format!("[VO:GENERATE:CONFIG] {}: {error}", config_path.display()))?;
    validate_config(&config)?;
    if config
        .generation
        .iter()
        .any(|generation| generation.executable.is_none())
    {
        vo_engine::prepare_path_dependencies(&project)
            .map_err(|error| format!("[VO:GENERATE:PROVIDER] {error}"))?;
    }
    let cache_root = generation_cache_root()?;
    let mut materialized = Vec::new();
    for generation in config.generation {
        materialized.extend(run_generation(&project, &cache_root, generation, options)?);
    }
    Ok(materialized)
}

pub(crate) fn generate_for_build(
    project: &Path,
) -> Result<Option<Vec<vo_engine::GeneratedSource>>, String> {
    if vo_engine::is_bytecode_artifact(project) {
        return Ok(None);
    }
    let project_root = if project.is_file() {
        project.parent().unwrap_or(project)
    } else {
        project
    };
    if !project_root.join("vo.generate.toml").is_file() {
        return Ok(None);
    }
    let options = Options {
        project: project_root.to_path_buf(),
        write: false,
        target: host_target(),
    };
    let outputs = generate_project(&options)?;
    let mut generated = Vec::new();
    for output in outputs {
        if output.relative_path.extension() != Some(OsStr::new("vo")) {
            continue;
        }
        let bytes = fs::read(&output.destination).map_err(|error| {
            format!(
                "[VO:GENERATE:OUTPUT] failed to read {}: {error}",
                output.destination.display()
            )
        })?;
        generated.push(
            vo_engine::GeneratedSource::new(output.relative_path, bytes)
                .map_err(|error| format!("[VO:GENERATE:OUTPUT] {error}"))?,
        );
    }
    Ok(Some(generated))
}

fn validate_config(config: &GenerateFile) -> Result<(), String> {
    if config.format != 1 {
        return Err("[VO:GENERATE:CONFIG] format must be 1".to_string());
    }
    if config.generation.is_empty() {
        return Err("[VO:GENERATE:CONFIG] generation must contain at least one entry".to_string());
    }
    let mut identities = BTreeSet::new();
    for generation in &config.generation {
        if generation.provider.is_empty()
            || generation.version.is_empty()
            || generation.schema_kind.is_empty()
            || generation.outputs.is_empty()
        {
            return Err(
                "[VO:GENERATE:CONFIG] provider identity and outputs are required".to_string(),
            );
        }
        if !identities.insert((
            generation.provider.as_str(),
            generation.version.as_str(),
            generation.schema.as_str(),
        )) {
            return Err("[VO:GENERATE:CONFIG] duplicate generator entry".to_string());
        }
        validate_generated_path(&generation.schema)
            .map_err(|detail| format!("[VO:GENERATE:CONFIG] schema: {detail}"))?;
        let mut outputs = BTreeSet::new();
        for output in &generation.outputs {
            validate_generated_path(output)
                .map_err(|detail| format!("[VO:GENERATE:CONFIG] output {output}: {detail}"))?;
            if !outputs.insert(output) {
                return Err(format!(
                    "[VO:GENERATE:CONFIG] duplicate output path {output}"
                ));
            }
        }
    }
    Ok(())
}

fn run_generation(
    project: &Path,
    cache_root: &Path,
    mut generation: Generation,
    options: &Options,
) -> Result<Vec<MaterializedOutput>, String> {
    generation.capabilities.sort();
    generation.capabilities.dedup();
    let executable = resolve_provider_executable(project, &generation, &options.target)?;
    verify_provider(&executable, &generation)?;
    let schema_path = project.join(&generation.schema);
    let schema = fs::read(&schema_path).map_err(|error| {
        format!(
            "[VO:GENERATE:SCHEMA] failed to read {}: {error}",
            schema_path.display()
        )
    })?;
    let identity = GeneratorIdentity {
        name: generation.provider.clone(),
        version: generation.version.clone(),
        schema_kind: generation.schema_kind.clone(),
    };
    let cache_key = generator_cache_key(&GeneratorCacheInput {
        identity: &identity,
        schema_fingerprint: schema_source_fingerprint(&schema),
        toolchain: vo_module::TOOLCHAIN_VERSION,
        target: &options.target,
        capabilities: &generation.capabilities,
    });
    let cache_entry = cache_root.join(hex(&cache_key));
    let staging = temporary_staging_root()?;
    fs::create_dir_all(&staging).map_err(|error| {
        format!(
            "[VO:GENERATE:STAGING] failed to create {}: {error}",
            staging.display()
        )
    })?;
    let result = invoke_provider(
        &executable,
        &schema_path,
        &staging,
        &options.target,
        &generation.capabilities,
    )
    .and_then(|()| {
        let destination_root = if options.write { project } else { &cache_entry };
        materialize_outputs(
            &staging,
            destination_root,
            &generation.outputs,
            options.write,
        )
    });
    let _ = fs::remove_dir_all(&staging);
    result
}

fn verify_provider(executable: &Path, generation: &Generation) -> Result<(), String> {
    let output = Command::new(executable)
        .arg("describe")
        .stdin(Stdio::null())
        .output()
        .map_err(|error| {
            format!(
                "[VO:GENERATE:PROVIDER] failed to describe {}: {error}",
                executable.display()
            )
        })?;
    if !output.status.success() {
        return Err(format!(
            "[VO:GENERATE:PROVIDER] describe failed for {}: {}",
            executable.display(),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let description = String::from_utf8(output.stdout)
        .map_err(|_| "[VO:GENERATE:PROVIDER] describe output must be UTF-8".to_string())?;
    let expected = [
        "protocol=vo.generator-provider/1".to_string(),
        format!("name={}", generation.provider),
        format!("version={}", generation.version),
        format!("schema_kind={}", generation.schema_kind),
    ];
    for line in expected {
        if !description.lines().any(|candidate| candidate == line) {
            return Err(format!(
                "[VO:GENERATE:PROVIDER] {} did not attest {line}",
                executable.display()
            ));
        }
    }
    Ok(())
}

fn invoke_provider(
    executable: &Path,
    schema: &Path,
    output_root: &Path,
    target: &str,
    capabilities: &[String],
) -> Result<(), String> {
    let mut command = Command::new(executable);
    command
        .arg("generate")
        .arg("--schema")
        .arg(schema)
        .arg("--output-root")
        .arg(output_root)
        .arg("--toolchain")
        .arg(vo_module::TOOLCHAIN_VERSION)
        .arg("--target")
        .arg(target)
        .stdin(Stdio::null());
    for capability in capabilities {
        command.arg("--capability").arg(capability);
    }
    let output = command.output().map_err(|error| {
        format!(
            "[VO:GENERATE:PROVIDER] failed to execute {}: {error}",
            executable.display()
        )
    })?;
    if !output.status.success() {
        return Err(format!(
            "[VO:GENERATE:PROVIDER] {} failed: {}",
            executable.display(),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(())
}

fn materialize_outputs(
    staging: &Path,
    destination_root: &Path,
    outputs: &[String],
    write: bool,
) -> Result<Vec<MaterializedOutput>, String> {
    let mut materialized = Vec::with_capacity(outputs.len());
    for output in outputs {
        let source = staging.join(output);
        let bytes = fs::read(&source).map_err(|error| {
            format!(
                "[VO:GENERATE:OUTPUT] provider omitted declared output {}: {error}",
                source.display()
            )
        })?;
        let destination = destination_root.join(output);
        if !write && destination.exists() {
            let existing = fs::read(&destination).map_err(|error| {
                format!(
                    "[VO:GENERATE:CACHE] failed to read {}: {error}",
                    destination.display()
                )
            })?;
            if existing != bytes {
                return Err(format!(
                    "[VO:GENERATE:CACHE] immutable cache collision at {}",
                    destination.display()
                ));
            }
        } else {
            write_atomically(&destination, &bytes)?;
        }
        materialized.push(MaterializedOutput {
            relative_path: PathBuf::from(output),
            destination,
        });
    }
    Ok(materialized)
}

fn write_atomically(path: &Path, bytes: &[u8]) -> Result<(), String> {
    let parent = path
        .parent()
        .ok_or_else(|| "[VO:GENERATE:OUTPUT] output has no parent".to_string())?;
    fs::create_dir_all(parent).map_err(|error| {
        format!(
            "[VO:GENERATE:OUTPUT] failed to create {}: {error}",
            parent.display()
        )
    })?;
    let name = path
        .file_name()
        .ok_or_else(|| "[VO:GENERATE:OUTPUT] output has no file name".to_string())?;
    let temporary = parent.join(format!(".{}.tmp-{}", name.to_string_lossy(), process::id()));
    let mut file = fs::OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(&temporary)
        .map_err(|error| {
            format!(
                "[VO:GENERATE:OUTPUT] failed to create {}: {error}",
                temporary.display()
            )
        })?;
    if let Err(error) = file.write_all(bytes).and_then(|()| file.sync_all()) {
        let _ = fs::remove_file(&temporary);
        return Err(format!(
            "[VO:GENERATE:OUTPUT] failed to write {}: {error}",
            temporary.display()
        ));
    }
    if let Err(error) = fs::rename(&temporary, path) {
        let _ = fs::remove_file(&temporary);
        return Err(format!(
            "[VO:GENERATE:OUTPUT] failed to commit {}: {error}",
            path.display()
        ));
    }
    Ok(())
}

fn absolute_project_root(path: &Path) -> Result<PathBuf, String> {
    let path = if path.is_file() {
        path.parent().unwrap_or(path)
    } else {
        path
    };
    path.canonicalize().map_err(|error| {
        format!(
            "[VO:GENERATE:PROJECT] failed to resolve {}: {error}",
            path.display()
        )
    })
}

fn resolve_development_executable(project: &Path, executable: &str) -> PathBuf {
    let executable = Path::new(executable);
    if executable.is_absolute() {
        executable.to_path_buf()
    } else {
        project.join(executable)
    }
}

fn resolve_provider_executable(
    project: &Path,
    generation: &Generation,
    target: &str,
) -> Result<PathBuf, String> {
    if let Some(executable) = generation.executable.as_deref() {
        return Ok(resolve_development_executable(project, executable));
    }
    let plan = vo_module::project::read_project_plan_at_root(project)
        .map_err(|error| format!("[VO:GENERATE:PROVIDER] {error}"))?;
    let cache_root = vo_engine::default_mod_cache_root()
        .map_err(|error| format!("[VO:GENERATE:PROVIDER] {error}"))?;
    let cache_fs = vo_common::vfs::RealFs::new(&cache_root);
    let mut resolved = None;
    for locked in plan.locked_modules() {
        let module_dir =
            vo_module::cache::layout::relative_module_dir(&locked.path, &locked.version);
        let extension = vo_module::cache::validate::validate_installed_extension_manifest(
            &cache_fs,
            &module_dir,
            locked,
        )
        .map_err(|error| format!("[VO:GENERATE:PROVIDER] {error}"))?;
        let Some(extension) = extension else {
            continue;
        };
        for provider in extension.generators {
            if provider.name != generation.provider
                || provider.version != generation.version
                || provider.schema_kind != generation.schema_kind
            {
                continue;
            }
            let artifact_name = provider.artifacts.get(target).ok_or_else(|| {
                format!(
                    "[VO:GENERATE:PROVIDER] {}@{} has no artifact for target {target}",
                    provider.name, provider.version
                )
            })?;
            let artifact_id = vo_module::identity::ArtifactId {
                kind: "extension-generator".to_string(),
                target: target.to_string(),
                name: artifact_name.clone(),
            };
            vo_module::cache::validate::validate_installed_artifact(
                &cache_fs,
                &module_dir,
                locked,
                &artifact_id,
            )
            .map_err(|error| format!("[VO:GENERATE:PROVIDER] {error}"))?;
            let relative_artifact = vo_module::artifact::artifact_relative_path(&artifact_id)
                .map_err(|error| format!("[VO:GENERATE:PROVIDER] {error}"))?;
            let executable = cache_root.join(&module_dir).join(relative_artifact);
            if resolved.replace(executable).is_some() {
                return Err(format!(
                    "[VO:GENERATE:PROVIDER] multiple dependencies provide {}@{} for {}",
                    generation.provider, generation.version, generation.schema_kind
                ));
            }
        }
    }
    resolved.ok_or_else(|| {
        format!(
            "[VO:GENERATE:PROVIDER] no authenticated dependency provides {}@{} for {} on {target}; add the provider module or use a development executable override",
            generation.provider, generation.version, generation.schema_kind
        )
    })
}

fn generation_cache_root() -> Result<PathBuf, String> {
    if let Some(path) = std::env::var_os("VO_GENERATOR_CACHE") {
        return Ok(PathBuf::from(path));
    }
    let module_cache = vo_engine::default_mod_cache_root()
        .map_err(|error| format!("[VO:GENERATE:CACHE] {error}"))?;
    let root = module_cache
        .parent()
        .unwrap_or(&module_cache)
        .join("generated-v1");
    Ok(root)
}

fn temporary_staging_root() -> Result<PathBuf, String> {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|error| format!("[VO:GENERATE:STAGING] clock error: {error}"))?
        .as_nanos();
    Ok(std::env::temp_dir().join(format!("vo-generate-{}-{nonce}", process::id())))
}

fn host_target() -> String {
    match (std::env::consts::ARCH, std::env::consts::OS) {
        ("aarch64", "macos") => "aarch64-apple-darwin".to_string(),
        ("x86_64", "macos") => "x86_64-apple-darwin".to_string(),
        ("aarch64", "linux") => "aarch64-unknown-linux-gnu".to_string(),
        ("x86_64", "linux") => "x86_64-unknown-linux-gnu".to_string(),
        ("x86_64", "windows") => "x86_64-pc-windows-msvc".to_string(),
        (architecture, operating_system) => {
            format!("{architecture}-unknown-{operating_system}")
        }
    }
}

fn hex(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut output = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        output.push(char::from(HEX[(byte >> 4) as usize]));
        output.push(char::from(HEX[(byte & 0x0f) as usize]));
    }
    output
}
