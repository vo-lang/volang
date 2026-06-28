use crate::command_lint::inferred_tools_for_command;
use crate::config::{load_toolchains, Tool};
use anyhow::{anyhow, bail, Result};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs;
use std::path::Path;

#[derive(Debug, Deserialize)]
struct TestsFile {
    version: u32,
    #[serde(default)]
    default_targets: Vec<String>,
    #[serde(default)]
    required_file_pass_targets: Vec<String>,
    gc_regression_alias: Option<String>,
    #[serde(default, rename = "alias")]
    aliases: Vec<TestAlias>,
    #[serde(default)]
    matrices: BTreeMap<String, TestMatrix>,
    #[serde(default, rename = "target")]
    targets: Vec<TestTarget>,
}

#[derive(Debug, Clone, Deserialize)]
struct TestAlias {
    name: String,
    targets: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct TestMatrix {
    targets: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TestTarget {
    pub(crate) name: String,
    pub(crate) kind: String,
    pub(crate) backend: String,
    #[serde(default)]
    pub(crate) env: BTreeMap<String, String>,
    pub(crate) default_timeout_sec: u64,
    #[serde(default)]
    pub(crate) build_command: Vec<String>,
    #[serde(default)]
    pub(crate) release_build_args: Vec<String>,
    #[serde(default)]
    pub(crate) runner_command: Vec<String>,
}

pub(crate) struct TestConfig {
    pub(crate) targets: HashMap<String, TestTarget>,
    pub(crate) aliases: HashMap<String, Vec<String>>,
    pub(crate) matrices: HashMap<String, Vec<String>>,
    pub(crate) default_targets: Vec<String>,
    pub(crate) required_file_pass_targets: Vec<String>,
    pub(crate) gc_regression_targets: Vec<String>,
}

pub(crate) fn required_tools_for_test_targets(
    root: &Path,
    target_specs: &[String],
) -> Result<BTreeSet<String>> {
    let config = load_test_config(root)?;
    let targets = if target_specs.is_empty() {
        config.default_targets.clone()
    } else {
        resolve_target_specs(&config, target_specs.to_vec())?
    };
    let mut tools = BTreeSet::new();
    for target_name in targets {
        let Some(target) = config.targets.get(&target_name) else {
            bail!("unknown test target: {target_name}");
        };
        tools.extend(inferred_tools_for_test_command(
            &target.name,
            "build_command",
            &target.build_command,
        )?);
        tools.extend(inferred_tools_for_test_command(
            &target.name,
            "runner_command",
            &target.runner_command,
        )?);
    }
    Ok(tools)
}

pub(crate) fn load_test_config(root: &Path) -> Result<TestConfig> {
    let text = fs::read_to_string(root.join("eng/tests.toml"))?;
    let parsed: TestsFile = toml::from_str(&text)?;
    if parsed.version != 1 {
        bail!("eng/tests.toml version must be 1");
    }
    let toolchains = load_toolchains(root)?;
    let mut targets = HashMap::new();
    for target in parsed.targets {
        validate_test_selector_name("target", &target.name)?;
        if !matches!(
            target.kind.as_str(),
            "native" | "embed" | "wasm" | "compile"
        ) {
            bail!(
                "eng/tests.toml target {} has invalid kind {}",
                target.name,
                target.kind
            );
        }
        if target.backend.trim().is_empty() {
            bail!(
                "eng/tests.toml target {} backend cannot be empty",
                target.name
            );
        }
        if target.default_timeout_sec == 0 {
            bail!(
                "eng/tests.toml target {} default_timeout_sec must be > 0",
                target.name
            );
        }
        for key in target.env.keys() {
            if key.trim().is_empty() || key.trim() != key {
                bail!(
                    "eng/tests.toml target {} has invalid empty/whitespace env key",
                    target.name
                );
            }
        }
        validate_optional_command(&target.name, "build_command", &target.build_command)?;
        validate_optional_command(
            &target.name,
            "release_build_args",
            &target.release_build_args,
        )?;
        validate_optional_command(&target.name, "runner_command", &target.runner_command)?;
        validate_test_command_tools(
            &toolchains.tools,
            &target.name,
            "build_command",
            &target.build_command,
        )?;
        validate_test_command_tools(
            &toolchains.tools,
            &target.name,
            "runner_command",
            &target.runner_command,
        )?;
        match target.kind.as_str() {
            "wasm" => {
                if target.build_command.is_empty() {
                    bail!(
                        "eng/tests.toml wasm target {} must declare build_command",
                        target.name
                    );
                }
                if target.runner_command.is_empty() {
                    bail!(
                        "eng/tests.toml wasm target {} must declare runner_command",
                        target.name
                    );
                }
            }
            _ => {
                if !target.runner_command.is_empty() || !target.build_command.is_empty() {
                    bail!(
                        "eng/tests.toml non-wasm target {} cannot declare build_command or runner_command",
                        target.name
                    );
                }
                if !target.release_build_args.is_empty() {
                    bail!(
                        "eng/tests.toml non-wasm target {} cannot declare release_build_args",
                        target.name
                    );
                }
            }
        }
        if targets.insert(target.name.clone(), target).is_some() {
            bail!("duplicate test target in eng/tests.toml");
        }
    }
    let mut aliases = HashMap::new();
    for alias in parsed.aliases {
        validate_test_selector_name("alias", &alias.name)?;
        if targets.contains_key(&alias.name) {
            bail!(
                "eng/tests.toml alias {} conflicts with a target name",
                alias.name
            );
        }
        if alias.targets.is_empty() {
            bail!(
                "eng/tests.toml alias {} targets cannot be empty",
                alias.name
            );
        }
        for target in &alias.targets {
            if !targets.contains_key(target) {
                bail!(
                    "eng/tests.toml alias {} references unknown target {}",
                    alias.name,
                    target
                );
            }
        }
        if aliases.insert(alias.name.clone(), alias.targets).is_some() {
            bail!("duplicate test alias in eng/tests.toml");
        }
    }
    let mut matrices = HashMap::new();
    for (name, matrix) in parsed.matrices {
        validate_test_selector_name("matrix", &name)?;
        if targets.contains_key(&name)
            && !(matrix.targets.len() == 1 && matrix.targets.first() == Some(&name))
        {
            bail!("eng/tests.toml matrix {name} conflicts with a target name");
        }
        if matrix.targets.is_empty() {
            bail!("eng/tests.toml matrix {name} targets cannot be empty");
        }
        validate_target_refs(&format!("matrix {name} targets"), &matrix.targets, &targets)?;
        if let Some(alias_targets) = aliases.get(&name) {
            if alias_targets != &matrix.targets {
                bail!(
                    "eng/tests.toml matrix {name} conflicts with alias {name}; matching compatibility aliases are allowed"
                );
            }
        }
        if matrices.insert(name, matrix.targets).is_some() {
            bail!("duplicate test matrix in eng/tests.toml");
        }
    }
    if parsed.default_targets.is_empty() {
        bail!("eng/tests.toml default_targets cannot be empty");
    }
    validate_target_refs("default_targets", &parsed.default_targets, &targets)?;
    if parsed.required_file_pass_targets.is_empty() {
        bail!("eng/tests.toml required_file_pass_targets cannot be empty");
    }
    validate_target_refs(
        "required_file_pass_targets",
        &parsed.required_file_pass_targets,
        &targets,
    )?;
    let gc_regression_alias = parsed
        .gc_regression_alias
        .as_deref()
        .ok_or_else(|| anyhow!("eng/tests.toml gc_regression_alias is required"))?;
    validate_test_selector_name("gc_regression_alias", gc_regression_alias)?;
    let gc_regression_targets = aliases.get(gc_regression_alias).cloned().ok_or_else(|| {
        anyhow!("eng/tests.toml gc_regression_alias references unknown alias {gc_regression_alias}")
    })?;
    Ok(TestConfig {
        targets,
        aliases,
        matrices,
        default_targets: parsed.default_targets,
        required_file_pass_targets: parsed.required_file_pass_targets,
        gc_regression_targets,
    })
}

fn validate_target_refs(
    field: &str,
    values: &[String],
    targets: &HashMap<String, TestTarget>,
) -> Result<()> {
    for target in values {
        if !targets.contains_key(target) {
            bail!("eng/tests.toml {field} references unknown target {target}");
        }
    }
    Ok(())
}

fn validate_test_selector_name(kind: &str, name: &str) -> Result<()> {
    if name.trim().is_empty() {
        bail!("eng/tests.toml {kind} name cannot be empty");
    }
    if name.trim() != name {
        bail!("eng/tests.toml {kind} {name} has surrounding whitespace");
    }
    if !name
        .chars()
        .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || matches!(ch, '-' | '_'))
    {
        bail!(
            "eng/tests.toml {kind} {name} must use lowercase ASCII letters, digits, hyphen, or underscore"
        );
    }
    Ok(())
}

fn validate_optional_command(target: &str, field: &str, values: &[String]) -> Result<()> {
    for value in values {
        if value.trim().is_empty() || value.trim() != value {
            bail!("eng/tests.toml target {target} {field} contains empty or padded argument");
        }
    }
    Ok(())
}

fn validate_test_command_tools(
    declared_tools: &BTreeMap<String, Tool>,
    target: &str,
    field: &str,
    values: &[String],
) -> Result<()> {
    for tool in inferred_tools_for_test_command(target, field, values)? {
        if !declared_tools.contains_key(&tool) {
            bail!(
                "eng/tests.toml target {target} {field} uses undeclared tool {tool}; declare it in eng/toolchains.toml"
            );
        }
    }
    Ok(())
}

fn inferred_tools_for_test_command(
    target: &str,
    field: &str,
    values: &[String],
) -> Result<BTreeSet<String>> {
    if values.is_empty() {
        return Ok(BTreeSet::new());
    }
    let tools = inferred_tools_for_command(values);
    if tools.is_empty() {
        bail!(
            "eng/tests.toml target {target} {field} uses unsupported executable {}; teach vo-dev to infer its tool",
            values[0]
        );
    }
    Ok(tools.into_iter().map(str::to_string).collect())
}

pub(crate) fn resolve_target_specs(config: &TestConfig, specs: Vec<String>) -> Result<Vec<String>> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for spec in specs {
        let expanded = if let Some(targets) = config.aliases.get(&spec) {
            targets.clone()
        } else if let Some(targets) = config.matrices.get(&spec) {
            targets.clone()
        } else if config.targets.contains_key(&spec) {
            vec![spec.clone()]
        } else {
            bail!("unknown test target, alias, or matrix: {spec}");
        };
        for target in expanded {
            if seen.insert(target.clone()) {
                out.push(target);
            }
        }
    }
    Ok(out)
}
