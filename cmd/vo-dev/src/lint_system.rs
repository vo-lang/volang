use crate::artifact_lint::lint_artifacts;
use crate::artifact_repo_lint::path_matches_artifact;
use crate::command_lint::{
    inferred_tools_for_command, validate_embedded_test_task_tools,
    validate_first_party_run_node_workspace,
};
use crate::config::{
    load_artifacts, load_ci, load_project, load_tasks, load_toolchains, ProjectRepo, Task,
    TaskFile, TaskGroup,
};
use crate::lint_policy::{
    contains_glob_meta, declared_repo_names, validate_ascii_slug, validate_repo_path_like,
    validate_structured_input_reference, validate_unique_values,
};
use crate::release_system;
use crate::task_graph::{resolve_selector, task_map, task_tools_recursive};
use crate::task_planner::git_lines;
use crate::task_runner::{current_vm_production_source_state_hash, final_gate_selectors};
use crate::tool_lint::lint_toolchain_file;
use anyhow::{anyhow, bail, Result};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) fn cmd_lint(root: &Path, args: Vec<String>) -> Result<()> {
    let opts = LintArgs::parse(args)?;
    let target = opts.target.as_str();
    match target {
        "tasks" => {
            lint_tasks(root, opts.strict)?;
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
        "evidence" => {
            lint_vm_production_gate_evidence(root)?;
            println!("vo-dev lint evidence: ok");
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
            lint_tasks(root, opts.strict)?;
            lint_artifacts(root)?;
            lint_repo_boundaries(root)?;
            lint_layout(root)?;
            lint_docs(root)?;
            lint_vm_production_gate_evidence(root)?;
            lint_examples(root)?;
            lint_benchmarks(root)?;
            release_system::lint_release(root)?;
            println!("vo-dev lint all: ok");
        }
        other => bail!("unknown lint target: {other}"),
    }
    Ok(())
}

struct LintArgs {
    target: String,
    strict: bool,
}

impl LintArgs {
    fn parse(args: Vec<String>) -> Result<Self> {
        let mut target = "all".to_string();
        let mut target_seen = false;
        let mut strict = false;
        for arg in args {
            match arg.as_str() {
                "--strict" => strict = true,
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
        Ok(Self { target, strict })
    }
}

fn lint_tasks(root: &Path, strict: bool) -> Result<()> {
    let config = load_tasks(root)?;
    lint_task_file_with_options(root, &config, strict)?;
    lint_first_party_checkout_history(root, &config)
}

const FIRST_PARTY_HISTORY_WORKFLOWS: [&str; 3] = [
    ".github/workflows/module-system-enforcement.yml",
    ".github/workflows/production-readiness.yml",
    ".github/workflows/deploy-site.yml",
];

fn lint_first_party_checkout_history(root: &Path, config: &TaskFile) -> Result<()> {
    let eng_lint_tasks = config
        .tasks
        .iter()
        .find(|task| task.name == "eng-lint-tasks")
        .ok_or_else(|| anyhow!("eng/tasks.toml missing required task eng-lint-tasks"))?;
    for relative in FIRST_PARTY_HISTORY_WORKFLOWS {
        if !eng_lint_tasks.inputs.iter().any(|input| input == relative) {
            bail!("eng-lint-tasks inputs must include {relative} because task lint validates first-party checkout history");
        }
        let source = fs::read_to_string(root.join(relative))
            .map_err(|error| anyhow!("could not read {relative}: {error}"))?;
        lint_first_party_checkout_history_source(relative, &source)?;
    }
    Ok(())
}

fn lint_first_party_checkout_history_source(relative: &str, source: &str) -> Result<()> {
    for repo in ["vogui", "voplay", "vopack", "vostore", "BlockKart"] {
        let marker = format!("      - name: Checkout {repo}\n");
        let (_, tail) = source
            .split_once(&marker)
            .ok_or_else(|| anyhow!("{relative} is missing first-party checkout {repo}"))?;
        let block = tail.split("\n      - name:").next().unwrap_or(tail);
        if !block.lines().any(|line| line.trim() == "fetch-depth: 0") {
            bail!("{relative} checkout {repo} must use fetch-depth: 0 so provenance can verify locked historical commits");
        }
    }
    Ok(())
}

pub(crate) fn lint_task_file(root: &Path, config: &TaskFile) -> Result<()> {
    lint_task_file_with_options(root, config, false)
}

pub(crate) fn lint_task_file_with_options(
    root: &Path,
    config: &TaskFile,
    strict: bool,
) -> Result<()> {
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
    let group_meta = group_metadata_map(config)?;
    lint_stdlib_embedded_source_inputs(&task_map)?;
    lint_vo_dev_source_contract_consumer_inputs(&task_map)?;
    lint_vm_hardening_tasks_run_unfiltered_crate_tests(&task_map)?;
    lint_vm_jit_manager_surface(root)?;
    lint_playground_host_wake_task_filter(&task_map)?;
    lint_voplay_industrial_gate_policy(root, config, &task_map)?;

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
        validate_unique_values("task", &task.name, "Linux package", &task.linux_packages)?;
        for package in &task.linux_packages {
            if package.is_empty()
                || !package
                    .chars()
                    .all(|ch| ch.is_ascii_alphanumeric() || matches!(ch, '+' | '-' | '.'))
            {
                bail!(
                    "task {} Linux package {package:?} must be a non-empty Debian package name",
                    task.name
                );
            }
        }
        validate_unique_values("task", &task.name, "tag", &task.tags)?;
        for tag in &task.tags {
            validate_ascii_slug("task tag", tag, &['-', '_', '.'])?;
        }
        if let Some(owner) = &task.owner {
            validate_ascii_slug("task owner", owner, &['-', '_', '.'])?;
        }
        if strict && task.owner.as_deref().unwrap_or_default().trim().is_empty() {
            bail!(
                "task {} missing owner; add owner = \"<subsystem>\" to eng/tasks.toml",
                task.name
            );
        }
        if strict && task.tags.is_empty() {
            bail!(
                "task {} missing tags; add tags = [\"<surface>\", \"<domain>\"]",
                task.name
            );
        }
        if strict && !task.tags.iter().any(|tag| is_surface_tag(tag)) {
            bail!(
                "task {} missing surface tag; add one of {}",
                task.name,
                SURFACE_TAGS.join(",")
            );
        }
        if strict
            && matches!(task.tier.as_str(), "contract" | "stress")
            && !task.tags.iter().any(|tag| tag == &task.tier)
        {
            bail!(
                "task {} tier {} must also carry tag {}",
                task.name,
                task.tier,
                task.tier
            );
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
        if strict && !group_meta.contains_key(group.as_str()) {
            bail!("group {group} missing [[group]] metadata in eng/tasks.toml");
        }
    }
    for group in group_meta.values() {
        lint_task_group_metadata(config, &task_map, group, strict)?;
    }
    if strict {
        lint_group_included_in_reverse_links(config, &group_meta)?;
        lint_required_groups(config)?;
        lint_public_task_reachability(config, &task_map)?;
        lint_selected_gate_tasks_have_timeouts(config, &task_map)?;
    }
    for task in task_map.keys() {
        detect_task_cycle(task, &task_map, &mut Vec::new(), &mut HashSet::new())?;
    }
    for group in config.groups.keys() {
        detect_group_cycle(group, config, &mut Vec::new(), &mut HashSet::new())?;
    }
    lint_vm_production_selects_vm_hardening_contract(config)?;
    lint_vm_production_selects_codegen_contract(config)?;
    lint_vm_production_selects_vo_dev_contract(config)?;
    lint_vm_production_selects_runtime_surface_contract(config)?;
    lint_vm_production_selects_ffi_contract(config)?;
    lint_vm_production_selects_docs_contract(config)?;
    lint_vm_production_selects_app_contract(config)?;
    lint_ci_file(root, config, &task_map)?;
    lint_ci_selected_tool_provisioning(root, config, &tools)?;
    Ok(())
}

fn lint_stdlib_embedded_source_inputs(task_map: &BTreeMap<String, Task>) -> Result<()> {
    for task_name in ["cargo-test-stdlib", "cargo-test-web-runtime-wasm"] {
        let Some(task) = task_map.get(task_name) else {
            bail!("eng/tasks.toml missing required stdlib contract task {task_name}");
        };
        if !task.inputs.iter().any(|input| input == "lang/stdlib/**") {
            bail!(
                "task {task_name} must include input lang/stdlib/** because vo-stdlib embeds the source stdlib tree"
            );
        }
    }
    Ok(())
}

fn lint_vo_dev_source_contract_consumer_inputs(task_map: &BTreeMap<String, Task>) -> Result<()> {
    let Some(task) = task_map.get("cargo-test-vo-dev") else {
        bail!("eng/tasks.toml missing required vo-dev contract task cargo-test-vo-dev");
    };
    if !task
        .inputs
        .iter()
        .any(|input| input == "lang/crates/vo-source-contract/**")
    {
        bail!(
            "task cargo-test-vo-dev must include input lang/crates/vo-source-contract/** because vo-dev source-contract proofs consume that helper crate"
        );
    }
    Ok(())
}

const VM_HARDENING_UNFILTERED_CRATE_TESTS: &[(&str, &[&str])] = &[
    (
        "cargo-test-common-core-hardening",
        &["cargo", "test", "-p", "vo-common-core"],
    ),
    (
        "cargo-test-jit-hardening",
        &["cargo", "test", "-p", "vo-jit"],
    ),
    (
        "cargo-test-vo-source-contract",
        &["cargo", "test", "-p", "vo-source-contract"],
    ),
    ("cargo-test-vm-hardening", &["cargo", "test", "-p", "vo-vm"]),
    (
        "cargo-test-vm-hardening-jit",
        &["cargo", "test", "-p", "vo-vm", "--features", "jit"],
    ),
];

fn lint_vm_production_selects_vm_hardening_contract(config: &TaskFile) -> Result<()> {
    for group in ["contract", "vm-production", "pr"] {
        let Some(items) = config.groups.get(group) else {
            bail!("{group} group missing from task config");
        };
        if !items.iter().any(|item| item == "vm-hardening") {
            bail!("{group} must include vm-hardening");
        }
    }
    let selected = resolve_selector(config, "vm-hardening")?;
    for (required, _) in VM_HARDENING_UNFILTERED_CRATE_TESTS {
        if !selected.iter().any(|task| task == *required) {
            bail!("vm-hardening must select {required}");
        }
    }
    Ok(())
}

fn lint_vm_hardening_tasks_run_unfiltered_crate_tests(
    task_map: &BTreeMap<String, Task>,
) -> Result<()> {
    for (task_name, expected_command) in VM_HARDENING_UNFILTERED_CRATE_TESTS {
        let Some(task) = task_map.get(*task_name) else {
            bail!("eng/tasks.toml missing required VM hardening task {task_name}");
        };
        let expected: Vec<String> = expected_command
            .iter()
            .map(|part| (*part).to_string())
            .collect();
        if task.command != expected {
            bail!(
                "task {task_name} must run unfiltered crate tests {:?}; name filters can let blocker proofs drift out of vm-hardening",
                expected
            );
        }
    }
    Ok(())
}

fn lint_vm_jit_manager_surface(root: &Path) -> Result<()> {
    let source = fs::read_to_string(root.join("lang/crates/vo-vm/src/vm/mod.rs"))?;
    lint_vm_jit_manager_surface_in_source(&source)
}

fn lint_vm_jit_manager_surface_in_source(source: &str) -> Result<()> {
    for line in source.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("pub mod jit_mgr") {
            bail!(
                "jit_mgr must remain a private VM module; re-export only semantic public JIT types"
            );
        }
        if trimmed.starts_with("pub enum VmJitState") {
            bail!("VmJitState must remain VM-owned, not part of the public API");
        }
        if trimmed.starts_with("pub use jit_mgr::") && trimmed.contains("JitManager") {
            bail!("JitManager must remain VM-owned; expose JitConfig without re-exporting the manager");
        }
        if trimmed.starts_with("pub jit: VmJitState") {
            bail!("Vm.jit must remain VM-owned so strict JIT module validation cannot be bypassed");
        }
        if trimmed.starts_with("pub fn replace_extern_registry_for_testing") {
            bail!("Vm must not expose an extern registry replacement hook after module load");
        }
    }
    Ok(())
}

fn lint_selected_gate_tasks_have_timeouts(
    config: &TaskFile,
    task_map: &BTreeMap<String, Task>,
) -> Result<()> {
    for selector in final_gate_selectors(config)? {
        for task_name in resolve_selector(config, &selector)? {
            let Some(task) = task_map.get(&task_name) else {
                continue;
            };
            if task.timeout_sec.is_none() {
                bail!("{selector} selected task {} has no timeout_sec", task.name);
            }
        }
    }
    Ok(())
}

fn lint_vm_production_selects_codegen_contract(config: &TaskFile) -> Result<()> {
    for group in ["contract", "vm-production", "pr"] {
        let Some(items) = config.groups.get(group) else {
            bail!("{group} group missing from task config");
        };
        if !items.iter().any(|item| item == "codegen-contract") {
            bail!("{group} must include codegen-contract");
        }
    }
    let selected = resolve_selector(config, "codegen-contract")?;
    if !selected.iter().any(|task| task == "cargo-test-codegen") {
        bail!("codegen-contract must select cargo-test-codegen");
    }
    Ok(())
}

fn lint_vm_production_selects_vo_dev_contract(config: &TaskFile) -> Result<()> {
    for group in ["contract", "vm-production", "pr"] {
        let Some(items) = config.groups.get(group) else {
            bail!("{group} group missing from task config");
        };
        if !items.iter().any(|item| item == "vo-dev-contract") {
            bail!("{group} must include vo-dev-contract");
        }
    }
    let selected = resolve_selector(config, "vo-dev-contract")?;
    for required in ["eng-lint-tasks", "cargo-test-vo-dev", "cargo-test-vo-test"] {
        if !selected.iter().any(|task| task == required) {
            bail!("vo-dev-contract must select {required}");
        }
    }
    Ok(())
}

fn lint_vm_production_selects_runtime_surface_contract(config: &TaskFile) -> Result<()> {
    for group in ["contract", "vm-production", "pr"] {
        let Some(items) = config.groups.get(group) else {
            bail!("{group} group missing from task config");
        };
        if !items.iter().any(|item| item == "runtime-surface-contract") {
            bail!("{group} must include runtime-surface-contract");
        }
    }
    let selected = resolve_selector(config, "runtime-surface-contract")?;
    for required in [
        "cargo-check-vo-app-runtime",
        "cargo-test-vo-app-runtime",
        "cargo-test-vo-engine",
        "cargo-test-vo-playground-host-wake",
    ] {
        if !selected.iter().any(|task| task == required) {
            bail!("runtime-surface-contract must select {required}");
        }
    }
    Ok(())
}

fn lint_vm_production_selects_ffi_contract(config: &TaskFile) -> Result<()> {
    for group in ["contract", "vm-production", "pr"] {
        let Some(items) = config.groups.get(group) else {
            bail!("{group} group missing from task config");
        };
        if !items.iter().any(|item| item == "ffi-contract") {
            bail!("{group} must include ffi-contract");
        }
    }
    let selected = resolve_selector(config, "ffi-contract")?;
    for required in [
        "cargo-test-ffi-macro",
        "cargo-test-vo-ext",
        "cargo-check-vo-ext-wasm",
    ] {
        if !selected.iter().any(|task| task == required) {
            bail!("ffi-contract must select {required}");
        }
    }
    Ok(())
}

fn lint_vm_production_selects_docs_contract(config: &TaskFile) -> Result<()> {
    for group in ["contract", "vm-production"] {
        let Some(items) = config.groups.get(group) else {
            bail!("{group} group missing from task config");
        };
        if !items.iter().any(|item| item == "docs-contract") {
            bail!("{group} must include docs-contract");
        }
    }
    let selected = resolve_selector(config, "docs-contract")?;
    if !selected.iter().any(|task| task == "docs-lint") {
        bail!("docs-contract must select docs-lint");
    }
    lint_docs_lint_shell_inputs(config)?;
    Ok(())
}

fn lint_vm_production_selects_app_contract(config: &TaskFile) -> Result<()> {
    for group in ["contract", "vm-production", "pr"] {
        let Some(items) = config.groups.get(group) else {
            bail!("{group} group missing from task config");
        };
        if !items.iter().any(|item| item == "app-contract") {
            bail!("{group} must include app-contract");
        }
    }
    let selected = resolve_selector(config, "app-contract")?;
    for required in [
        "wasm-check",
        "cargo-test-web-hardening",
        "cargo-test-studio-wasm-source-contract",
        "cargo-test-web-runtime-wasm",
        "vo-test-wasm",
    ] {
        if !selected.iter().any(|task| task == required) {
            bail!("app-contract must select {required}");
        }
    }
    lint_studio_wasm_source_contract_inputs(config)?;
    Ok(())
}

fn lint_studio_wasm_source_contract_inputs(config: &TaskFile) -> Result<()> {
    let Some(task) = config
        .tasks
        .iter()
        .find(|task| task.name == "cargo-test-studio-wasm-source-contract")
    else {
        bail!("cargo-test-studio-wasm-source-contract task missing from task config");
    };
    for required in [
        "apps/studio/wasm/src/lib.rs",
        "apps/studio/src/lib/studio_wasm.ts",
    ] {
        if !task.inputs.iter().any(|input| input == required) {
            bail!("cargo-test-studio-wasm-source-contract inputs must include {required}");
        }
    }
    Ok(())
}

fn lint_docs_lint_shell_inputs(config: &TaskFile) -> Result<()> {
    let Some(task) = config.tasks.iter().find(|task| task.name == "docs-lint") else {
        bail!("docs-lint task missing from task config");
    };
    for required_tool in ["node", "rust", "vo-dev"] {
        if !task.tools.iter().any(|tool| tool == required_tool) {
            bail!("docs-lint tools must include {required_tool}");
        }
    }
    for required in [
        "cmd/vo-dev/**",
        "eng/tasks.toml",
        "scripts/ci/docs_lint.mjs",
        "scripts/ci/docs_sync.mjs",
        "lang/docs/spec/**",
        "lang/docs/dev/**",
        "lang/docs/dev-notes/**",
        "lang/docs/vo-for-gophers.md",
        "apps/playground-legacy/src/assets/docs/generated/**",
        "apps/studio/docs/manifest.toml",
        "apps/studio/docs/pages/**",
    ] {
        if !task.inputs.iter().any(|input| input == required) {
            bail!("docs-lint inputs must include {required}");
        }
    }
    Ok(())
}

fn lint_ci_selected_tool_provisioning(
    root: &Path,
    config: &TaskFile,
    tools: &crate::config::ToolchainFile,
) -> Result<()> {
    let pr_tasks = resolve_selector(config, "pr")?;
    let mut selected_tools = BTreeSet::new();
    for task in pr_tasks {
        selected_tools.extend(task_tools_recursive(root, &task)?);
    }
    for name in selected_tools {
        let Some(tool) = tools.tools.get(&name) else {
            bail!("CI-selected tool {name} is not declared in eng/toolchains.toml");
        };
        if tool.required == Some(false) || ci_workflow_provisions_tool(&name, tool) {
            continue;
        }
        bail!(
            "CI-selected required tool {name} has no provisioning path; add a bootstrap command in eng/toolchains.toml or teach the workflow/vo-dev policy to provision it"
        );
    }
    Ok(())
}

fn ci_workflow_provisions_tool(name: &str, tool: &crate::config::Tool) -> bool {
    matches!(
        name,
        "rust" | "python" | "node" | "npm" | "vo-dev" | "wasm-pack"
    ) || tool.bootstrap.is_some()
}

fn lint_ci_file(root: &Path, config: &TaskFile, task_map: &BTreeMap<String, Task>) -> Result<()> {
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
    lint_vm_readiness_changed_prefixes(&ci.known_prefix)?;
    lint_vm_readiness_changed_prefix_scopes(&ci.known_prefix, config)?;
    Ok(())
}

const VM_READINESS_CHANGED_PREFIX_TASKS: &[(&str, &[&str])] = &[
    (
        "lang/crates/vo-vm/**",
        &[
            "vo-test-runtime-contract",
            "vo-test-jit-contract",
            "cargo-test-vm-hardening",
            "cargo-test-vm-hardening-jit",
        ],
    ),
    (
        "lang/crates/vo-runtime/**",
        &[
            "cargo-test-runtime",
            "cargo-test-gc-runtime",
            "vo-test-runtime-contract",
            "vo-test-gc",
            "cargo-test-vm-hardening",
            "cargo-test-vm-hardening-jit",
        ],
    ),
    (
        "lang/crates/vo-jit/**",
        &[
            "vo-test-runtime-contract",
            "vo-test-jit-contract",
            "cargo-test-jit-hardening",
            "cargo-test-vm-hardening-jit",
        ],
    ),
    (
        "lang/crates/vo-source-contract/**",
        &[
            "cargo-test-vo-source-contract",
            "cargo-test-vo-dev",
            "cargo-test-runtime",
            "cargo-test-jit-hardening",
            "cargo-test-vm-hardening",
            "cargo-test-vm-hardening-jit",
        ],
    ),
    (
        "lang/crates/vo-analysis/**",
        &["cargo-test-analysis", "vo-test-compile"],
    ),
    (
        "lang/crates/vo-codegen/**",
        &[
            "vo-test-runtime-contract",
            "vo-test-jit-contract",
            "vo-test-osr",
            "cargo-test-codegen",
            "vo-test-compile",
        ],
    ),
    (
        "tests/lang/**",
        &[
            "test-data-lint",
            "vo-test-compile",
            "vo-test",
            "vo-test-osr",
            "vo-test-nostd",
            "vo-test-wasm",
            "vo-test-gc",
        ],
    ),
    (
        "lang/stdlib/**",
        &[
            "cargo-test-stdlib",
            "cargo-test-web-runtime-wasm",
            "vo-test-runtime-contract",
            "vo-test-wasm",
        ],
    ),
    (
        "lang/crates/vo-ffi-macro/**",
        &[
            "cargo-test-ffi-macro",
            "cargo-test-vo-ext",
            "cargo-check-vo-ext-wasm",
        ],
    ),
    (
        "lang/crates/vo-ext/**",
        &[
            "cargo-test-vo-ext",
            "cargo-check-vo-ext-wasm",
            "cargo-test-ffi-macro",
        ],
    ),
    (
        "lang/crates/vo-app-runtime/**",
        &["cargo-check-vo-app-runtime", "cargo-test-vo-app-runtime"],
    ),
    ("lang/crates/vo-engine/**", &["cargo-test-vo-engine"]),
    (
        "lang/crates/vo-web/**",
        &[
            "wasm-check",
            "vo-test-wasm",
            "cargo-test-web-hardening",
            "cargo-test-web-runtime-wasm",
        ],
    ),
    (
        "apps/playground-legacy/rust/**",
        &["cargo-test-vo-playground-host-wake"],
    ),
    (
        "apps/studio/**",
        &[
            "docs-lint",
            "studio-build",
            "cargo-test-studio-wasm-source-contract",
        ],
    ),
    ("cmd/vo-dev/**", &["eng-lint-tasks", "cargo-test-vo-dev"]),
    ("eng/tasks.toml", &["eng-lint-tasks", "cargo-test-vo-dev"]),
    ("eng/ci.toml", &["eng-lint-tasks", "cargo-test-vo-dev"]),
    (".github/workflows/**", &["docs-lint", "ci-self-check"]),
    (
        "scripts/ci/**",
        &[
            "eng-lint-tasks",
            "docs-lint",
            "ci-self-check",
            "quickplay-validate",
        ],
    ),
];

const VM_PRODUCTION_READINESS_CHANGED_PREFIX_TASKS: &[(&str, &[&str])] = &[
    (
        "lang/crates/vo-analysis/**",
        &["cargo-test-analysis", "vo-test-compile"],
    ),
    (
        "tests/lang/**",
        &[
            "test-data-lint",
            "vo-test-compile",
            "vo-test",
            "vo-test-osr",
            "vo-test-nostd",
            "vo-test-wasm",
            "vo-test-gc",
        ],
    ),
];

fn lint_vm_readiness_changed_prefixes(prefixes: &[crate::config::KnownPrefix]) -> Result<()> {
    for (path, required_tasks) in VM_READINESS_CHANGED_PREFIX_TASKS {
        let Some(prefix) = prefixes.iter().find(|prefix| prefix.path == *path) else {
            bail!("eng/ci.toml missing VM readiness changed-mode prefix {path}");
        };
        for required in *required_tasks {
            if !prefix.tasks.iter().any(|task| task == required) {
                bail!(
                    "eng/ci.toml known_prefix {path} must select {required} for VM readiness changed-mode coverage"
                );
            }
        }
    }
    Ok(())
}

fn lint_vm_readiness_changed_prefix_scopes(
    prefixes: &[crate::config::KnownPrefix],
    config: &TaskFile,
) -> Result<()> {
    let pr_scope: BTreeSet<_> = resolve_selector(config, "pr")?.into_iter().collect();
    for (path, required_tasks) in VM_READINESS_CHANGED_PREFIX_TASKS {
        if !prefixes.iter().any(|prefix| prefix.path == *path) {
            continue;
        }
        for required in *required_tasks {
            if !pr_scope.contains(*required) {
                bail!(
                    "eng/tasks.toml pr scope must include {required} selected by VM readiness changed-mode prefix {path}"
                );
            }
        }
    }
    let vm_production_scope: BTreeSet<_> = resolve_selector(config, "vm-production")?
        .into_iter()
        .collect();
    for (path, required_tasks) in VM_PRODUCTION_READINESS_CHANGED_PREFIX_TASKS {
        if !prefixes.iter().any(|prefix| prefix.path == *path) {
            continue;
        }
        for required in *required_tasks {
            if !vm_production_scope.contains(*required) {
                bail!(
                    "eng/tasks.toml vm-production scope must include {required} selected by VM readiness changed-mode prefix {path}"
                );
            }
        }
    }
    Ok(())
}

fn lint_playground_host_wake_task_filter(task_map: &BTreeMap<String, Task>) -> Result<()> {
    let Some(task) = task_map.get("cargo-test-vo-playground-host-wake") else {
        bail!("cargo-test-vo-playground-host-wake task missing from task config");
    };
    if !task.command.iter().any(|arg| arg == "host_wake") {
        bail!(
            "cargo-test-vo-playground-host-wake must use host_wake filter so all host wake proofs run"
        );
    }
    Ok(())
}

const VOPLAY_INDUSTRIAL_GATE_SCRIPT_INPUTS: &[&str] = &[
    "scripts/ci/voplay_industrial_readiness.mjs",
    "scripts/ci/voplay_render_stress.mjs",
    "scripts/ci/voplay_render_architecture_lint.mjs",
    "scripts/ci/blockkart_engine_boundary_lint.mjs",
];

const VOPLAY_REQUIRED_SOURCE_FACTS: &[&str] = &[
    "render_pipeline_stages_constructed",
    "frame_orchestrator_stage_only",
    "framegraph_dispatch_owns_pass_execution",
    "resource_registry_owns_all_targets",
    "batch_plan_real_bounds",
    "batch_plan_real_lod_inputs",
    "batch_plan_real_culling_counters",
    "batch_plan_scene_wired",
    "batch_plan_terrain_decal_real_entries",
    "physics_surface_source_no_track_position_inference",
    "physics_set_pose_backend_only",
    "physics_pose_reset_helper_backend_only",
    "physics_replay_records_backend_apply_hash",
    "blockkart_product_boundary",
    "blockkart_no_direct_player_physics_mutation",
    "blockkart_no_direct_entity_physics_mutation",
    "evidence_has_no_unresolved_next_fix",
];

const VOPLAY_RENDER_ARCHITECTURE_FAILURE_CODES: &[&str] = &[
    "renderer.execute_render_node_macro",
    "framegraph.pipeline_stage_unused",
    "render_world.zero_bounds",
    "render_world.seed_workload_lod",
    "render_world.frustum_counters_not_mutated",
    "render_world.distance_counters_not_mutated",
    "render_world.terrain_batch_unwired",
    "render_world.decal_batch_unwired",
    "vehicle.track_position_surface_inference",
    "contact.track_position_surface_inference",
    "telemetry.track_position_surface_inference",
    "vehicle.set_pose_direct_physics_mutation",
    "vehicle.pose_reset_helper_direct_physics_mutation",
    "vehicle.backend_apply_contract_not_used",
    "replay.backend_apply_hash_missing",
    "blockkart.primitive_authoring_owner",
    "blockkart.low_level_hud_facts",
    "blockkart.visual_mutable_vehicle_state",
    "blockkart.direct_vehicle_set_pose",
    "blockkart.direct_player_physics_mutation",
    "blockkart.direct_entity_physics_mutation",
];

const VOPLAY_BLOCKKART_BOUNDARY_FAILURE_CODES: &[&str] = &[
    "voplay.vehicle_track_position_surface_inference",
    "voplay.contact_track_position_surface_inference",
    "voplay.telemetry_track_position_surface_inference",
    "voplay.set_pose_direct_physics_mutation",
    "voplay.pose_reset_helper_direct_physics_mutation",
    "voplay.backend_apply_contract_not_used",
    "voplay.replay_backend_apply_hash_missing",
    "blockkart.primitive_authoring_owner",
    "blockkart.low_level_hud_facts",
    "blockkart.visual_mutable_vehicle_state",
    "blockkart.direct_vehicle_set_pose",
    "blockkart.direct_player_physics_mutation",
    "blockkart.direct_entity_physics_mutation",
];

fn lint_voplay_industrial_gate_policy(
    root: &Path,
    config: &TaskFile,
    task_map: &BTreeMap<String, Task>,
) -> Result<()> {
    lint_voplay_industrial_gate_task_wiring(config, task_map)?;
    let readiness = read_gate_policy_script(root, VOPLAY_INDUSTRIAL_GATE_SCRIPT_INPUTS[0])?;
    let render_stress = read_gate_policy_script(root, VOPLAY_INDUSTRIAL_GATE_SCRIPT_INPUTS[1])?;
    let architecture = read_gate_policy_script(root, VOPLAY_INDUSTRIAL_GATE_SCRIPT_INPUTS[2])?;
    let blockkart_boundary =
        read_gate_policy_script(root, VOPLAY_INDUSTRIAL_GATE_SCRIPT_INPUTS[3])?;
    lint_voplay_industrial_gate_sources(
        &readiness,
        &render_stress,
        &architecture,
        &blockkart_boundary,
    )
}

fn read_gate_policy_script(root: &Path, relative: &str) -> Result<String> {
    let path = root.join(relative);
    fs::read_to_string(&path)
        .map_err(|err| anyhow!("could not read voplay industrial gate script {relative}: {err}"))
}

fn lint_voplay_industrial_gate_task_wiring(
    config: &TaskFile,
    task_map: &BTreeMap<String, Task>,
) -> Result<()> {
    let site_scope: BTreeSet<_> = resolve_selector(config, "site")?.into_iter().collect();
    for required in [
        "voplay-industrial-source-audit",
        "voplay-industrial-readiness-report",
        "voplay-industrial-readiness",
    ] {
        if !site_scope.contains(required) {
            bail!("site scope must include {required} through voplay-industrial final gate");
        }
    }

    let app_site_scope: BTreeSet<_> = resolve_selector(config, "app-site")?.into_iter().collect();
    for required in [
        "voplay-render-architecture-lint",
        "blockkart-engine-boundary-lint",
        "voplay-render-stress-budgeted",
        "voplay-render-soak-10m",
        "voplay-physics-industrial-stress",
    ] {
        if !app_site_scope.contains(required) {
            bail!("app-site scope must include {required} for voplay industrial gate coverage");
        }
    }

    let Some(eng_lint_tasks) = task_map.get("eng-lint-tasks") else {
        bail!("eng/tasks.toml missing required task eng-lint-tasks");
    };
    for required in VOPLAY_INDUSTRIAL_GATE_SCRIPT_INPUTS {
        if !eng_lint_tasks.inputs.iter().any(|input| input == required) {
            bail!("eng-lint-tasks inputs must include {required} because task lint reads voplay industrial gate source sentinels");
        }
    }
    Ok(())
}

fn lint_voplay_industrial_gate_sources(
    readiness: &str,
    render_stress: &str,
    architecture: &str,
    blockkart_boundary: &str,
) -> Result<()> {
    require_gate_source_tokens(
        "scripts/ci/voplay_industrial_readiness.mjs",
        readiness,
        &[
            "sourceFactRequirements",
            "evidenceTable",
            "sourceAuditFailures",
            "firstPrinciplesVerdict",
            "addRequiredSourceFact(",
            "addEvidenceRow(",
            "const requiredFalseFacts = sourceFactRequirements",
            ".filter((fact) => fact.required && fact.status !== true)",
            "const unresolvedEvidenceNextFixes = evidenceTable",
            "'source_facts.required_all_pass'",
            "const industrialReady = failures.length === 0",
            "strictMode: !allowNotReady",
            "if (!industrialReady && !allowNotReady)",
            "## Evidence Table",
        ],
    )?;
    require_gate_source_tokens(
        "scripts/ci/voplay_industrial_readiness.mjs",
        readiness,
        VOPLAY_REQUIRED_SOURCE_FACTS,
    )?;

    require_gate_source_tokens(
        "scripts/ci/voplay_render_stress.mjs",
        render_stress,
        &[
            "'render.perf_gate_failed'",
            "'render.p90_over_budget'",
            "'render.p99_over_budget'",
            "'render.slow_frames_over_budget'",
            "'summary.p90_over_budget'",
            "'summary.p99_over_budget'",
            "'summary.slow_frames_over_budget'",
            "p1 += summaryIssues.filter((issue) => issue.severity === 1).length",
            "p1Enforced: !runtimeProbeOnly",
            "status: p0 === 0 && (runtimeProbeOnly || p1 === 0) ? 'pass' : 'fail'",
            "if (report.status !== 'pass')",
        ],
    )?;
    reject_gate_source_tokens(
        "scripts/ci/voplay_render_stress.mjs",
        render_stress,
        &[
            "&& !hostPacingOnly",
            "if (hostPacingOnly)",
            "hostPacingOnly ?",
        ],
    )?;

    require_gate_source_tokens(
        "scripts/ci/voplay_render_architecture_lint.mjs",
        architecture,
        VOPLAY_RENDER_ARCHITECTURE_FAILURE_CODES,
    )?;
    require_gate_source_tokens(
        "scripts/ci/voplay_render_architecture_lint.mjs",
        architecture,
        &[
            "constructsRuntimeStage(rendererAuditSource, token)",
            "execute_render_node!",
            "SurfaceMaterialAtTrackPosition",
            "Body\\.SetPosition",
            "applyPoseResetToBackend",
            "ApplyVehicleForces",
            "PrimitiveStats",
            "primitive3d\\.NewBuilder",
            "w\\.player\\.SetVelocity",
        ],
    )?;

    require_gate_source_tokens(
        "scripts/ci/blockkart_engine_boundary_lint.mjs",
        blockkart_boundary,
        VOPLAY_BLOCKKART_BOUNDARY_FAILURE_CODES,
    )?;
    require_gate_source_tokens(
        "scripts/ci/blockkart_engine_boundary_lint.mjs",
        blockkart_boundary,
        &[
            "SurfaceMaterialAtTrackPosition",
            "Body\\.SetPosition",
            "applyPoseResetToBackend",
            "ApplyVehicleForces",
            "BackendApplyHash",
            "PrimitiveStats",
            "w\\.vehicle\\.SetPose",
            "primitive3d\\.NewBuilder",
            "w\\.player\\.SetVelocity",
            "directEntityMutation",
        ],
    )?;
    Ok(())
}

fn require_gate_source_tokens(script: &str, source: &str, required: &[&str]) -> Result<()> {
    for token in required {
        if !source.contains(token) {
            bail!("{script} must keep voplay industrial gate sentinel {token:?}");
        }
    }
    Ok(())
}

fn reject_gate_source_tokens(script: &str, source: &str, forbidden: &[&str]) -> Result<()> {
    for token in forbidden {
        if source.contains(token) {
            bail!("{script} must not weaken voplay industrial gate with sentinel {token:?}");
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

const SURFACE_TAGS: &[&str] = &[
    "manifest-lint",
    "lang-case",
    "crate-unit",
    "crate-integration",
    "contract",
    "model",
    "docs-policy",
    "example-smoke",
    "benchmark",
    "app-build",
    "app-smoke",
    "release-verify",
    "legacy-excluded",
    "tooling",
    "repo-policy",
];

const REQUIRED_GROUPS: &[&str] = &[
    "quality",
    "lang-main",
    "lang-backends",
    "compile-contract",
    "gc-contract",
    "jit-contract",
    "runtime-contract",
    "runtime-surface-contract",
    "typechecker-contract",
    "codegen-contract",
    "module-contract",
    "release-contract",
    "docs-contract",
    "app-contract",
    "docs",
    "examples",
    "benchmarks",
    "app-site",
    "release-verify",
    "legacy-excluded",
    "contract",
    "stress",
    "vm-production",
    "test",
    "site",
    "full",
    "pr",
];

const TOP_LEVEL_GROUPS: &[&str] = &[
    "pr",
    "full",
    "quality",
    "test",
    "contract",
    "stress",
    "vm-production",
    "site",
    "release-verify",
    "legacy-excluded",
];

fn is_surface_tag(tag: &str) -> bool {
    SURFACE_TAGS.contains(&tag)
}

fn group_metadata_map(config: &TaskFile) -> Result<BTreeMap<&str, &TaskGroup>> {
    let mut out = BTreeMap::new();
    for group in &config.group_meta {
        validate_ascii_slug("group name", &group.name, &['-'])?;
        if out.insert(group.name.as_str(), group).is_some() {
            bail!("duplicate [[group]] metadata for {}", group.name);
        }
    }
    Ok(out)
}

fn lint_task_group_metadata(
    config: &TaskFile,
    task_map: &BTreeMap<String, Task>,
    group: &TaskGroup,
    strict: bool,
) -> Result<()> {
    if group.title.trim().is_empty() {
        bail!("group {} title cannot be empty", group.name);
    }
    if group.tier_intent.trim().is_empty() {
        bail!("group {} tier_intent cannot be empty", group.name);
    }
    validate_ascii_slug("group owner", &group.owner, &['-', '_', '.'])?;
    validate_unique_values("group metadata", &group.name, "tag", &group.tags)?;
    validate_unique_values("group metadata", &group.name, "task", &group.tasks)?;
    validate_unique_values(
        "group metadata",
        &group.name,
        "included_in",
        &group.included_in,
    )?;
    for tag in &group.tags {
        validate_ascii_slug("group tag", tag, &['-', '_', '.'])?;
    }
    for item in &group.tasks {
        if !task_map.contains_key(item) && !config.groups.contains_key(item) {
            bail!(
                "group metadata {} references unknown task or group {item}",
                group.name
            );
        }
    }
    for parent in &group.included_in {
        if !config.groups.contains_key(parent) {
            bail!(
                "group metadata {} included_in references unknown group {parent}",
                group.name
            );
        }
    }
    if group.selection_policy.trim().is_empty() {
        bail!("group {} selection_policy cannot be empty", group.name);
    }
    if strict {
        if group.tags.is_empty() {
            bail!("group {} missing tags", group.name);
        }
        let Some(items) = config.groups.get(&group.name) else {
            bail!("group metadata {} has no [groups] entry", group.name);
        };
        if items != &group.tasks {
            bail!(
                "group metadata {} tasks must match [groups] entry",
                group.name
            );
        }
    }
    Ok(())
}

fn lint_group_included_in_reverse_links(
    config: &TaskFile,
    group_meta: &BTreeMap<&str, &TaskGroup>,
) -> Result<()> {
    let mut expected_parents: BTreeMap<&str, BTreeSet<&str>> = config
        .groups
        .keys()
        .map(|group| (group.as_str(), BTreeSet::new()))
        .collect();
    for (parent, items) in &config.groups {
        for item in items {
            if !config.groups.contains_key(item) {
                continue;
            }
            if let Some(parents) = expected_parents.get_mut(item.as_str()) {
                parents.insert(parent.as_str());
            }
        }
    }
    let mut errors = Vec::new();
    for (group, metadata) in group_meta {
        let expected = expected_parents
            .get(group)
            .cloned()
            .unwrap_or_else(BTreeSet::new);
        let actual: BTreeSet<&str> = metadata.included_in.iter().map(String::as_str).collect();
        for parent in expected.difference(&actual) {
            errors.push(format!(
                "{group} included_in must include parent group {parent}"
            ));
        }
        for parent in actual.difference(&expected) {
            errors.push(format!(
                "{group} included_in must not include non-parent group {parent}"
            ));
        }
    }
    if !errors.is_empty() {
        bail!(
            "group metadata included_in must match direct [groups] parents: {}",
            errors.join(", ")
        );
    }
    Ok(())
}

fn lint_required_groups(config: &TaskFile) -> Result<()> {
    for group in REQUIRED_GROUPS {
        if !config.groups.contains_key(*group) {
            bail!("required group {group} missing from eng/tasks.toml");
        }
    }
    Ok(())
}

fn lint_public_task_reachability(
    config: &TaskFile,
    task_map: &BTreeMap<String, Task>,
) -> Result<()> {
    let mut reachable = HashSet::new();
    for group in TOP_LEVEL_GROUPS {
        if config.groups.contains_key(*group) {
            collect_group_reachable(config, group, &mut reachable, &mut Vec::new())?;
        }
    }
    for task in task_map.values() {
        if !task.internal && !reachable.contains(&task.name) {
            bail!(
                "public task {} is not reachable from a top-level task group",
                task.name
            );
        }
    }
    Ok(())
}

fn collect_group_reachable(
    config: &TaskFile,
    group: &str,
    reachable: &mut HashSet<String>,
    stack: &mut Vec<String>,
) -> Result<()> {
    if stack.iter().any(|item| item == group) {
        stack.push(group.to_string());
        bail!(
            "group cycle while checking reachability: {}",
            stack.join(" -> ")
        );
    }
    stack.push(group.to_string());
    if let Some(items) = config.groups.get(group) {
        for item in items {
            if config.groups.contains_key(item) {
                collect_group_reachable(config, item, reachable, stack)?;
            } else {
                reachable.insert(item.clone());
            }
        }
    }
    stack.pop();
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

#[derive(Debug, Deserialize)]
struct VmProductionGateEvidence {
    schema: String,
    selector: String,
    changed: bool,
    passed: bool,
    source_state: String,
    tasks: Vec<String>,
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
    lint_voplay_plan_status(root)?;

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

fn lint_voplay_plan_status(root: &Path) -> Result<()> {
    let active_plan = "voplay-code-engineering-quality-plan.md";
    let expected_superseded_by = format!("lang/docs/dev/{active_plan}");
    let dev_dir = root.join("lang/docs/dev");
    let mut saw_active_plan = false;
    let mut active_plans = Vec::new();
    for entry in fs::read_dir(&dev_dir)
        .map_err(|err| anyhow!("could not read {}: {err}", dev_dir.display()))?
    {
        let entry = entry.map_err(|err| anyhow!("could not read docs dir entry: {err}"))?;
        let file_name = entry.file_name().to_string_lossy().to_string();
        if !file_name.starts_with("voplay-") || !file_name.ends_with("-plan.md") {
            continue;
        }
        let path = entry.path();
        let text = fs::read_to_string(&path)
            .map_err(|err| anyhow!("could not read voplay plan {file_name}: {err}"))?;
        let status = doc_metadata_value(&text, "Status").unwrap_or_default();
        if status == "active" {
            active_plans.push(file_name.clone());
        }
        if file_name == active_plan {
            saw_active_plan = true;
            if status != "active" {
                bail!("{active_plan} must be the active voplay quality plan");
            }
            continue;
        }
        if status != "superseded" {
            bail!("old voplay plan {file_name} must be superseded by {expected_superseded_by}");
        }
        if doc_metadata_value(&text, "Superseded-By").as_deref()
            != Some(expected_superseded_by.as_str())
        {
            bail!(
                "old voplay plan {file_name} must declare Superseded-By: {expected_superseded_by}"
            );
        }
        let superseded_date = doc_metadata_value(&text, "Superseded-Date").unwrap_or_default();
        if superseded_date.trim().is_empty() {
            bail!("old voplay plan {file_name} must declare Superseded-Date");
        }
    }
    if !saw_active_plan {
        bail!("{active_plan} is missing");
    }
    if active_plans != [active_plan.to_string()] {
        bail!(
            "exactly one active voplay plan is allowed; found {}",
            active_plans.join(", ")
        );
    }
    Ok(())
}

fn doc_metadata_value(text: &str, key: &str) -> Option<String> {
    let prefix = format!("{key}:");
    text.lines()
        .find_map(|line| line.strip_prefix(&prefix))
        .map(str::trim)
        .map(ToOwned::to_owned)
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

fn lint_vm_production_gate_evidence(root: &Path) -> Result<()> {
    let config = load_tasks(root)?;
    let selectors = final_gate_selectors(&config)?;
    let expected_source_state = format!(
        "vm-production-current-source:{}",
        current_vm_production_source_state_hash(root)?
    );
    let evidence_dir = root.join("lang/docs/dev/vm-production-gate-evidence");
    if !evidence_dir.is_dir() {
        bail!("VM production gate evidence directory is missing");
    }
    let mut expected_files = BTreeSet::new();
    for selector in &selectors {
        let path = evidence_dir.join(format!("{selector}.json"));
        expected_files.insert(format!("{selector}.json"));
        let text = fs::read_to_string(&path)
            .map_err(|err| anyhow!("could not read VM production evidence {selector}: {err}"))?;
        let evidence: VmProductionGateEvidence = serde_json::from_str(&text)
            .map_err(|err| anyhow!("could not parse VM production evidence {selector}: {err}"))?;
        if evidence.schema != "vo-dev-task-run-evidence-v1" {
            bail!(
                "VM production evidence {selector} has invalid schema {}",
                evidence.schema
            );
        }
        if evidence.selector != *selector {
            bail!(
                "VM production evidence {} records selector {}",
                path.display(),
                evidence.selector
            );
        }
        if evidence.changed {
            bail!(
                "VM production evidence {selector} must be a full selector run, not changed mode"
            );
        }
        if !evidence.passed {
            bail!("VM production evidence {selector} did not pass");
        }
        if evidence.source_state != expected_source_state {
            bail!(
                "VM production evidence {selector} has stale source_state {}; expected {expected_source_state}",
                evidence.source_state
            );
        }
        let expected_tasks = resolve_selector(&config, selector)?;
        if evidence.tasks != expected_tasks {
            bail!(
                "VM production evidence {selector} task list is stale; rerun `cargo run -q -p vo-dev -- task run {selector}` after source changes"
            );
        }
    }
    for entry in fs::read_dir(&evidence_dir)
        .map_err(|err| anyhow!("could not read {}: {err}", evidence_dir.display()))?
    {
        let entry = entry.map_err(|err| anyhow!("could not read evidence dir entry: {err}"))?;
        let name = entry.file_name().to_string_lossy().to_string();
        if name.ends_with(".json") && !expected_files.contains(&name) {
            bail!("unexpected VM production evidence file: {name}");
        }
    }
    let readiness_path = root.join("lang/docs/dev/vm-production-readiness.md");
    let readiness = fs::read_to_string(&readiness_path)
        .map_err(|err| anyhow!("could not read {}: {err}", readiness_path.display()))?;
    let top_status = readiness
        .split("\n## ")
        .next()
        .unwrap_or(readiness.as_str());
    if top_status.contains("signoff\nremains withdrawn")
        || top_status.contains("signoff remains withdrawn")
        || top_status.contains("signoff is withdrawn")
    {
        bail!("VM production readiness top status still claims signoff is withdrawn");
    }
    if !top_status.contains(&expected_source_state) {
        bail!("VM production readiness top status must mention current evidence {expected_source_state}");
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

#[cfg(test)]
mod tests;
