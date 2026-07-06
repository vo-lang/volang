use super::*;

fn task(name: &str) -> Task {
    Task {
        name: name.to_string(),
        title: name.to_string(),
        command: vec!["cargo".to_string(), "test".to_string()],
        tools: vec![],
        node_workspaces: vec![],
        inputs: vec!["Cargo.toml".to_string()],
        outputs: vec![],
        tier: "contract".to_string(),
        tags: vec!["contract".to_string(), "crate-unit".to_string()],
        owner: Some("test".to_string()),
        cwd: None,
        env: BTreeMap::new(),
        needs: vec![],
        repo: None,
        repos: vec![],
        internal: false,
        timeout_sec: Some(60),
        platforms: vec![],
        shell: false,
    }
}

fn task_with_inputs(name: &str, inputs: &[&str]) -> Task {
    let mut task = task(name);
    task.inputs = inputs.iter().map(|input| input.to_string()).collect();
    task
}

fn task_without_timeout(name: &str) -> Task {
    let mut task = task(name);
    task.timeout_sec = None;
    task
}

fn final_selectors() -> Vec<String> {
    ["contract", "vm-production", "site", "release-verify"]
        .into_iter()
        .map(str::to_string)
        .collect()
}

fn task_group(name: &str, tasks: &[&str], included_in: &[&str]) -> TaskGroup {
    TaskGroup {
        name: name.to_string(),
        title: name.to_string(),
        tier_intent: "test group".to_string(),
        owner: "eng".to_string(),
        tags: vec!["contract".to_string()],
        tasks: tasks.iter().map(|item| item.to_string()).collect(),
        included_in: included_in.iter().map(|item| item.to_string()).collect(),
        selection_policy: "test selection".to_string(),
    }
}

#[test]
fn lint_group_metadata_included_in_must_match_parent_groups_062() {
    let mut groups = BTreeMap::new();
    groups.insert("parent".to_string(), vec!["child".to_string()]);
    groups.insert("child".to_string(), vec!["leaf-task".to_string()]);
    let mut config = TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![
            task_group("parent", &["child"], &[]),
            task_group("child", &["leaf-task"], &[]),
        ],
        tasks: vec![task("leaf-task")],
    };
    let group_meta = group_metadata_map(&config).unwrap();

    let err = lint_group_included_in_reverse_links(&config, &group_meta).unwrap_err();

    assert!(format!("{err:#}").contains("child included_in must include parent group parent"));
    config.group_meta[1].included_in.push("parent".to_string());
    let group_meta = group_metadata_map(&config).unwrap();
    lint_group_included_in_reverse_links(&config, &group_meta).unwrap();
}

#[test]
fn lint_group_metadata_included_in_rejects_extra_parent_links_062() {
    let mut groups = BTreeMap::new();
    groups.insert("parent".to_string(), vec!["child".to_string()]);
    groups.insert("child".to_string(), vec!["leaf-task".to_string()]);
    let config = TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![
            task_group("parent", &["child"], &[]),
            task_group("child", &["leaf-task"], &["parent", "not-a-parent"]),
        ],
        tasks: vec![task("leaf-task")],
    };
    let group_meta = group_metadata_map(&config).unwrap();

    let err = lint_group_included_in_reverse_links(&config, &group_meta).unwrap_err();

    assert!(format!("{err:#}")
        .contains("child included_in must not include non-parent group not-a-parent"));
}

fn vm_hardening_task_map() -> BTreeMap<String, Task> {
    let mut map = BTreeMap::new();
    for (name, command) in VM_HARDENING_UNFILTERED_CRATE_TESTS {
        let mut task = task(name);
        task.command = command.iter().map(|part| (*part).to_string()).collect();
        map.insert((*name).to_string(), task);
    }
    map
}

fn known_prefix(path: &str, tasks: &[&str]) -> crate::config::KnownPrefix {
    crate::config::KnownPrefix {
        path: path.to_string(),
        tasks: tasks.iter().map(|task| task.to_string()).collect(),
    }
}

fn vm_readiness_prefixes() -> Vec<crate::config::KnownPrefix> {
    VM_READINESS_CHANGED_PREFIX_TASKS
        .iter()
        .map(|(path, tasks)| known_prefix(path, tasks))
        .collect()
}

#[test]
fn vm_final_evidence_local_probe_source_062_gitignore_locks_probe_artifact_rules() {
    let gitignore = include_str!("../../../../.gitignore");
    for required in [
        "*.rmeta",
        "/vo_*_probe",
        "/vo_scheduler_*",
        "/vo_macro_hardcoded_raw_wake",
    ] {
        assert!(
            gitignore.lines().any(|line| line.trim() == required),
            ".gitignore must keep local red-team probe artifact rule {required}"
        );
    }
}

fn vm_readiness_scope_task_file(excluded_selector: &str, excluded: Option<&str>) -> TaskFile {
    let mut names = BTreeSet::new();
    for (_, tasks) in VM_READINESS_CHANGED_PREFIX_TASKS {
        for task_name in *tasks {
            names.insert((*task_name).to_string());
        }
    }
    let scope_items = |selector: &str| {
        names
            .iter()
            .filter(|task_name| {
                selector != excluded_selector || excluded != Some(task_name.as_str())
            })
            .cloned()
            .collect::<Vec<_>>()
    };
    let mut groups = BTreeMap::new();
    groups.insert("pr".to_string(), scope_items("pr"));
    groups.insert("vm-production".to_string(), scope_items("vm-production"));
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: names.into_iter().map(|name| task(&name)).collect(),
    }
}

fn codegen_task_file(
    contract_items: Vec<&str>,
    vm_production_items: Vec<&str>,
    pr_items: Vec<&str>,
) -> TaskFile {
    let mut groups = BTreeMap::new();
    groups.insert(
        "codegen-contract".to_string(),
        vec!["cargo-test-codegen".to_string()],
    );
    groups.insert(
        "contract".to_string(),
        contract_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "vm-production".to_string(),
        vm_production_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "pr".to_string(),
        pr_items.into_iter().map(|item| item.to_string()).collect(),
    );
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![task("cargo-test-codegen")],
    }
}

fn vo_dev_task_file(
    contract_items: Vec<&str>,
    vm_production_items: Vec<&str>,
    pr_items: Vec<&str>,
    vo_dev_items: Vec<&str>,
) -> TaskFile {
    let mut groups = BTreeMap::new();
    groups.insert(
        "vo-dev-contract".to_string(),
        vo_dev_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "contract".to_string(),
        contract_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "vm-production".to_string(),
        vm_production_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "pr".to_string(),
        pr_items.into_iter().map(|item| item.to_string()).collect(),
    );
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![
            task("eng-lint-tasks"),
            task("cargo-test-vo-dev"),
            task("cargo-test-vo-test"),
        ],
    }
}

fn vm_hardening_task_file(
    contract_items: Vec<&str>,
    vm_production_items: Vec<&str>,
    pr_items: Vec<&str>,
    hardening_items: Vec<&str>,
) -> TaskFile {
    let mut groups = BTreeMap::new();
    groups.insert(
        "vm-hardening".to_string(),
        hardening_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "contract".to_string(),
        contract_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "vm-production".to_string(),
        vm_production_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "pr".to_string(),
        pr_items.into_iter().map(|item| item.to_string()).collect(),
    );
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: VM_HARDENING_UNFILTERED_CRATE_TESTS
            .iter()
            .map(|(name, _)| task(name))
            .collect(),
    }
}

fn runtime_surface_task_file(
    contract_items: Vec<&str>,
    vm_production_items: Vec<&str>,
    pr_items: Vec<&str>,
    runtime_surface_items: Vec<&str>,
) -> TaskFile {
    let mut groups = BTreeMap::new();
    groups.insert(
        "runtime-surface-contract".to_string(),
        runtime_surface_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "contract".to_string(),
        contract_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "vm-production".to_string(),
        vm_production_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "pr".to_string(),
        pr_items.into_iter().map(|item| item.to_string()).collect(),
    );
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![
            task("cargo-check-vo-app-runtime"),
            task("cargo-test-vo-app-runtime"),
            task("cargo-test-vo-engine"),
            task("cargo-test-vo-playground-host-wake"),
        ],
    }
}

fn ffi_task_file(
    contract_items: Vec<&str>,
    vm_production_items: Vec<&str>,
    pr_items: Vec<&str>,
    ffi_items: Vec<&str>,
) -> TaskFile {
    let mut groups = BTreeMap::new();
    groups.insert(
        "ffi-contract".to_string(),
        ffi_items.into_iter().map(|item| item.to_string()).collect(),
    );
    groups.insert(
        "contract".to_string(),
        contract_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "vm-production".to_string(),
        vm_production_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "pr".to_string(),
        pr_items.into_iter().map(|item| item.to_string()).collect(),
    );
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![
            task("cargo-test-ffi-macro"),
            task("cargo-test-vo-ext"),
            task("cargo-check-vo-ext-wasm"),
        ],
    }
}

fn docs_lint_task() -> Task {
    let mut task = task_with_inputs(
        "docs-lint",
        &[
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
        ],
    );
    task.tools = ["node", "rust", "vo-dev"]
        .iter()
        .map(|tool| (*tool).to_string())
        .collect();
    task
}

fn docs_task_file(
    contract_items: Vec<&str>,
    vm_production_items: Vec<&str>,
    docs_items: Vec<&str>,
) -> TaskFile {
    let mut groups = BTreeMap::new();
    groups.insert(
        "docs-contract".to_string(),
        docs_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "contract".to_string(),
        contract_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "vm-production".to_string(),
        vm_production_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![docs_lint_task()],
    }
}

fn app_contract_task_file(
    contract_items: Vec<&str>,
    vm_production_items: Vec<&str>,
    pr_items: Vec<&str>,
    app_items: Vec<&str>,
) -> TaskFile {
    let mut groups = BTreeMap::new();
    groups.insert(
        "app-contract".to_string(),
        app_items.into_iter().map(|item| item.to_string()).collect(),
    );
    groups.insert(
        "contract".to_string(),
        contract_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "vm-production".to_string(),
        vm_production_items
            .into_iter()
            .map(|item| item.to_string())
            .collect(),
    );
    groups.insert(
        "pr".to_string(),
        pr_items.into_iter().map(|item| item.to_string()).collect(),
    );
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![
            task("wasm-check"),
            task("cargo-test-web-hardening"),
            task_with_inputs(
                "cargo-test-studio-wasm-source-contract",
                &[
                    "apps/studio/wasm/src/lib.rs",
                    "apps/studio/src/lib/studio_wasm.ts",
                ],
            ),
            task("cargo-test-web-runtime-wasm"),
            task("vo-test-wasm"),
        ],
    }
}

fn studio_wasm_production_source(source: &str) -> &str {
    source
        .split("\n#[cfg(all(test, target_arch = \"wasm32\"))]")
        .next()
        .expect("studio wasm source should have a production prefix")
}

fn studio_wasm_function_source<'a>(source: &'a str, signature: &str) -> Result<&'a str> {
    let start = source
        .find(signature)
        .ok_or_else(|| anyhow!("studio wasm source missing {signature}"))?;
    let rest = &source[start..];
    let end = rest.find("\nfn ").unwrap_or(rest.len());
    Ok(&rest[..end])
}

fn ts_contains_active_line_062(source: &str, expected: &str) -> bool {
    source.lines().any(|line| line.trim() == expected)
}

fn assert_studio_wasm_verified_decoder_contract(source: &str) -> Result<()> {
    let source = studio_wasm_production_source(source);
    let decoder = studio_wasm_function_source(source, "fn decode_verified_module(")?;
    if !decoder.contains("vo_vm::bytecode::Module::deserialize(bytecode)") {
        bail!("decode_verified_module must deserialize the provided bytecode argument");
    }
    if !decoder.contains("vo_common_core::verifier::verify_module(&module)") {
        bail!("decode_verified_module must verify deserialized bytecode before reuse");
    }
    if !decoder.contains("invalid {label} bytecode") {
        bail!("decode_verified_module must preserve verifier failures in user-visible errors");
    }
    let raw_decode_count = source.matches("Module::deserialize").count();
    if raw_decode_count != 1 {
        bail!(
                "Studio wasm serialized module decoding must be centralized in decode_verified_module; found {raw_decode_count} raw decode sites"
            );
    }
    for signature in [
        "fn try_load_vfs_compile_cache(",
        "fn save_vfs_compile_cache(",
    ] {
        let function = studio_wasm_function_source(source, signature)?;
        if !function.contains("decode_verified_module(") {
            bail!("{signature} must validate serialized modules through decode_verified_module");
        }
    }
    Ok(())
}

fn assert_studio_wasm_pending_host_event_contract(
    rust_source: &str,
    ts_source: &str,
) -> Result<()> {
    let rust_source = studio_wasm_production_source(rust_source);
    let serializer = vo_source_contract::compact_region_between(
        rust_source,
        "fnpending_host_event_to_js(event:&PendingHostEvent)->Object{",
        "fnproject_context_options_from_workspace_discovery",
    )
    .ok_or_else(|| anyhow!("studio wasm source missing pending_host_event_to_js"))?;
    for required in [
        ("key", "JsValue::from_str(&event.key.encode())"),
        ("source", "JsValue::from_str(event.source.as_str())"),
        ("token", "JsValue::from_str(&event.token.to_string())"),
        ("delayMs", "JsValue::from_f64(event.delay_msasf64)"),
        ("replay", "JsValue::from_bool(event.replay)"),
    ] {
        let (field, value_source) = required;
        if !vo_source_contract::compact_contains(&serializer, value_source) {
            bail!("pending host event serializer must emit {field} from the VM-owned event field");
        }
    }

    let drain = vo_source_contract::compact_region_between(
        rust_source,
        "pubfntake_pending_host_events(&mutself)->js_sys::Array{",
        "pubfnwake_host_event_vm",
    )
    .ok_or_else(|| anyhow!("studio wasm source missing take_pending_host_events"))?;
    if !vo_source_contract::compact_contains(&drain, "self.runtime.take_pending_host_events()") {
        bail!("takePendingHostEvents must drain VM-owned pending host events");
    }
    if !vo_source_contract::compact_contains(&drain, "pending_host_event_to_js(&event)") {
        bail!("takePendingHostEvents must share pending host event serialization");
    }

    let poll = vo_source_contract::compact_region_between(
        rust_source,
        "pubfnpoll_pending_host_event()->JsValue{",
        "pubfnwake_host_event(",
    )
    .ok_or_else(|| anyhow!("studio wasm source missing poll_pending_host_event"))?;
    if !vo_source_contract::compact_contains(&poll, "guest.poll_pending_host_event()") {
        bail!("pollPendingHostEvent must poll VM-owned pending host events");
    }
    if !vo_source_contract::compact_contains(&poll, "pending_host_event_to_js(&event).into()") {
        bail!("pollPendingHostEvent must share pending host event serialization");
    }

    if !ts_contains_active_line_062(
            ts_source,
            "takePendingHostEvents(): Array<{ key: string; source: string; token: string; delayMs: number; replay: boolean }>;",
        ) {
            bail!("Studio wasm TypeScript facade must expose source and replay host event fields");
        }
    if !ts_contains_active_line_062(
            ts_source,
            "pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number; replay: boolean } | null;",
        ) {
            bail!("Studio wasm TypeScript facade must expose legacy source and replay host event fields");
        }
    Ok(())
}

#[test]
fn lint_codegen_contract_requires_group_in_vm_production() {
    let config = codegen_task_file(
        vec!["codegen-contract"],
        vec!["cargo-test-codegen"],
        vec!["codegen-contract"],
    );
    let err = lint_vm_production_selects_codegen_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("vm-production must include codegen-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_codegen_contract_requires_group_in_contract() {
    let config = codegen_task_file(
        vec!["cargo-test-codegen"],
        vec!["codegen-contract"],
        vec!["codegen-contract"],
    );
    let err = lint_vm_production_selects_codegen_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("contract must include codegen-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_codegen_contract_requires_group_in_pr() {
    let config = codegen_task_file(
        vec!["codegen-contract"],
        vec!["codegen-contract"],
        vec!["cargo-test-codegen"],
    );
    let err = lint_vm_production_selects_codegen_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("pr must include codegen-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_codegen_contract_accepts_group_ownership() {
    let config = codegen_task_file(
        vec!["codegen-contract"],
        vec!["codegen-contract"],
        vec!["codegen-contract"],
    );
    lint_vm_production_selects_codegen_contract(&config).unwrap();
}

#[test]
fn lint_vo_dev_contract_requires_group_in_vm_production() {
    let config = vo_dev_task_file(
        vec!["vo-dev-contract"],
        vec!["eng-lint-tasks"],
        vec!["vo-dev-contract"],
        vec!["eng-lint-tasks", "cargo-test-vo-dev"],
    );
    let err = lint_vm_production_selects_vo_dev_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("vm-production must include vo-dev-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_vo_dev_contract_requires_group_in_contract() {
    let config = vo_dev_task_file(
        vec!["eng-lint-tasks"],
        vec!["vo-dev-contract"],
        vec!["vo-dev-contract"],
        vec!["eng-lint-tasks", "cargo-test-vo-dev"],
    );
    let err = lint_vm_production_selects_vo_dev_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("contract must include vo-dev-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_vo_dev_contract_requires_group_in_pr_051() {
    let config = vo_dev_task_file(
        vec!["vo-dev-contract"],
        vec!["vo-dev-contract"],
        vec!["eng-lint-tasks"],
        vec!["eng-lint-tasks", "cargo-test-vo-dev"],
    );
    let err = lint_vm_production_selects_vo_dev_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("pr must include vo-dev-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_vo_dev_contract_requires_policy_tests() {
    let config = vo_dev_task_file(
        vec!["vo-dev-contract"],
        vec!["vo-dev-contract"],
        vec!["vo-dev-contract"],
        vec!["eng-lint-tasks"],
    );
    let err = lint_vm_production_selects_vo_dev_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("vo-dev-contract must select cargo-test-vo-dev"),
        "{err:#}"
    );
}

#[test]
fn lint_vo_dev_contract_requires_vo_test_runner_tests_060() {
    let config = vo_dev_task_file(
        vec!["vo-dev-contract"],
        vec!["vo-dev-contract"],
        vec!["vo-dev-contract"],
        vec!["eng-lint-tasks", "cargo-test-vo-dev"],
    );
    let err = lint_vm_production_selects_vo_dev_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("vo-dev-contract must select cargo-test-vo-test"),
        "{err:#}"
    );
}

#[test]
fn lint_vo_dev_contract_accepts_group_ownership() {
    let config = vo_dev_task_file(
        vec!["vo-dev-contract"],
        vec!["vo-dev-contract"],
        vec!["vo-dev-contract"],
        vec!["eng-lint-tasks", "cargo-test-vo-dev", "cargo-test-vo-test"],
    );
    lint_vm_production_selects_vo_dev_contract(&config).unwrap();
}

#[test]
fn lint_runtime_surface_contract_requires_group_in_vm_production() {
    let config = runtime_surface_task_file(
        vec!["runtime-surface-contract"],
        vec!["cargo-check-vo-app-runtime"],
        vec!["runtime-surface-contract"],
        vec![
            "cargo-check-vo-app-runtime",
            "cargo-test-vo-app-runtime",
            "cargo-test-vo-engine",
        ],
    );
    let err = lint_vm_production_selects_runtime_surface_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("vm-production must include runtime-surface-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_runtime_surface_contract_requires_group_in_contract() {
    let config = runtime_surface_task_file(
        vec!["cargo-check-vo-app-runtime"],
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec![
            "cargo-check-vo-app-runtime",
            "cargo-test-vo-app-runtime",
            "cargo-test-vo-engine",
        ],
    );
    let err = lint_vm_production_selects_runtime_surface_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("contract must include runtime-surface-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_runtime_surface_contract_requires_group_in_pr() {
    let config = runtime_surface_task_file(
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["cargo-check-vo-app-runtime"],
        vec![
            "cargo-check-vo-app-runtime",
            "cargo-test-vo-app-runtime",
            "cargo-test-vo-engine",
            "cargo-test-vo-playground-host-wake",
        ],
    );
    let err = lint_vm_production_selects_runtime_surface_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("pr must include runtime-surface-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_runtime_surface_contract_requires_app_runtime_check() {
    let config = runtime_surface_task_file(
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["cargo-test-vo-engine"],
    );
    let err = lint_vm_production_selects_runtime_surface_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}")
            .contains("runtime-surface-contract must select cargo-check-vo-app-runtime"),
        "{err:#}"
    );
}

#[test]
fn lint_runtime_surface_contract_requires_engine_test() {
    let config = runtime_surface_task_file(
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["cargo-check-vo-app-runtime", "cargo-test-vo-app-runtime"],
    );
    let err = lint_vm_production_selects_runtime_surface_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("runtime-surface-contract must select cargo-test-vo-engine"),
        "{err:#}"
    );
}

#[test]
fn lint_runtime_surface_contract_requires_app_runtime_test() {
    let config = runtime_surface_task_file(
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["cargo-check-vo-app-runtime", "cargo-test-vo-engine"],
    );
    let err = lint_vm_production_selects_runtime_surface_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}")
            .contains("runtime-surface-contract must select cargo-test-vo-app-runtime"),
        "{err:#}"
    );
}

#[test]
fn lint_runtime_surface_contract_requires_playground_host_wake_test() {
    let config = runtime_surface_task_file(
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec![
            "cargo-check-vo-app-runtime",
            "cargo-test-vo-app-runtime",
            "cargo-test-vo-engine",
        ],
    );
    let err = lint_vm_production_selects_runtime_surface_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}")
            .contains("runtime-surface-contract must select cargo-test-vo-playground-host-wake"),
        "{err:#}"
    );
}

#[test]
fn lint_runtime_surface_contract_accepts_group_ownership() {
    let config = runtime_surface_task_file(
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec!["runtime-surface-contract"],
        vec![
            "cargo-check-vo-app-runtime",
            "cargo-test-vo-app-runtime",
            "cargo-test-vo-engine",
            "cargo-test-vo-playground-host-wake",
        ],
    );
    lint_vm_production_selects_runtime_surface_contract(&config).unwrap();
}

#[test]
fn lint_ffi_contract_requires_group_in_vm_production_055() {
    let config = ffi_task_file(
        vec!["ffi-contract"],
        vec!["cargo-test-ffi-macro"],
        vec!["ffi-contract"],
        vec![
            "cargo-test-ffi-macro",
            "cargo-test-vo-ext",
            "cargo-check-vo-ext-wasm",
        ],
    );
    let err = lint_vm_production_selects_ffi_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("vm-production must include ffi-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_ffi_contract_requires_group_in_contract_055() {
    let config = ffi_task_file(
        vec!["cargo-test-ffi-macro"],
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec![
            "cargo-test-ffi-macro",
            "cargo-test-vo-ext",
            "cargo-check-vo-ext-wasm",
        ],
    );
    let err = lint_vm_production_selects_ffi_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("contract must include ffi-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_ffi_contract_requires_group_in_pr_055() {
    let config = ffi_task_file(
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec!["cargo-test-ffi-macro"],
        vec![
            "cargo-test-ffi-macro",
            "cargo-test-vo-ext",
            "cargo-check-vo-ext-wasm",
        ],
    );
    let err = lint_vm_production_selects_ffi_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("pr must include ffi-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_ffi_contract_requires_macro_test_055() {
    let config = ffi_task_file(
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec!["cargo-test-vo-ext", "cargo-check-vo-ext-wasm"],
    );
    let err = lint_vm_production_selects_ffi_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("ffi-contract must select cargo-test-ffi-macro"),
        "{err:#}"
    );
}

#[test]
fn lint_ffi_contract_requires_vo_ext_test_055() {
    let config = ffi_task_file(
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec!["cargo-test-ffi-macro", "cargo-check-vo-ext-wasm"],
    );
    let err = lint_vm_production_selects_ffi_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("ffi-contract must select cargo-test-vo-ext"),
        "{err:#}"
    );
}

#[test]
fn lint_ffi_contract_requires_vo_ext_wasm_check_056() {
    let config = ffi_task_file(
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec!["cargo-test-ffi-macro", "cargo-test-vo-ext"],
    );
    let err = lint_vm_production_selects_ffi_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("ffi-contract must select cargo-check-vo-ext-wasm"),
        "{err:#}"
    );
}

#[test]
fn lint_ffi_contract_accepts_group_ownership_055() {
    let config = ffi_task_file(
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec!["ffi-contract"],
        vec![
            "cargo-test-ffi-macro",
            "cargo-test-vo-ext",
            "cargo-check-vo-ext-wasm",
        ],
    );
    lint_vm_production_selects_ffi_contract(&config).unwrap();
}

#[test]
fn lint_docs_contract_requires_group_in_vm_production() {
    let config = docs_task_file(vec!["docs-contract"], vec!["docs-lint"], vec!["docs-lint"]);
    let err = lint_vm_production_selects_docs_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("vm-production must include docs-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_docs_contract_requires_group_in_contract() {
    let config = docs_task_file(vec!["docs-lint"], vec!["docs-contract"], vec!["docs-lint"]);
    let err = lint_vm_production_selects_docs_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("contract must include docs-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_docs_contract_requires_docs_lint() {
    let config = docs_task_file(vec!["docs-contract"], vec!["docs-contract"], vec![]);
    let err = lint_vm_production_selects_docs_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("docs-contract must select docs-lint"),
        "{err:#}"
    );
}

#[test]
fn lint_docs_contract_requires_docs_lint_source_fact_inputs() {
    let mut config = docs_task_file(
        vec!["docs-contract"],
        vec!["docs-contract"],
        vec!["docs-lint"],
    );
    let docs_lint = config
        .tasks
        .iter_mut()
        .find(|task| task.name == "docs-lint")
        .expect("docs-lint task");
    docs_lint.inputs.retain(|input| input != "eng/tasks.toml");

    let err = lint_vm_production_selects_docs_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("docs-lint inputs must include eng/tasks.toml"),
        "{err:#}"
    );
}

#[test]
fn lint_docs_contract_requires_docs_lint_vo_dev_source_input_060() {
    let mut config = docs_task_file(
        vec!["docs-contract"],
        vec!["docs-contract"],
        vec!["docs-lint"],
    );
    let docs_lint = config
        .tasks
        .iter_mut()
        .find(|task| task.name == "docs-lint")
        .expect("docs-lint task");
    docs_lint.inputs.retain(|input| input != "cmd/vo-dev/**");

    let err = lint_vm_production_selects_docs_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("docs-lint inputs must include cmd/vo-dev/**"),
        "{err:#}"
    );
}

#[test]
fn lint_docs_contract_requires_docs_lint_script_inputs_060() {
    for required in ["scripts/ci/docs_lint.mjs", "scripts/ci/docs_sync.mjs"] {
        let mut config = docs_task_file(
            vec!["docs-contract"],
            vec!["docs-contract"],
            vec!["docs-lint"],
        );
        let docs_lint = config
            .tasks
            .iter_mut()
            .find(|task| task.name == "docs-lint")
            .expect("docs-lint task");
        docs_lint.inputs.retain(|input| input != required);

        let err = lint_vm_production_selects_docs_contract(&config).unwrap_err();
        assert!(
            format!("{err:#}").contains(&format!("docs-lint inputs must include {required}")),
            "{err:#}"
        );
    }
}

#[test]
fn lint_docs_contract_requires_docs_lint_studio_docs_manifest_input() {
    let mut config = docs_task_file(
        vec!["docs-contract"],
        vec!["docs-contract"],
        vec!["docs-lint"],
    );
    let docs_lint = config
        .tasks
        .iter_mut()
        .find(|task| task.name == "docs-lint")
        .expect("docs-lint task");
    docs_lint
        .inputs
        .retain(|input| input != "apps/studio/docs/manifest.toml");

    let err = lint_vm_production_selects_docs_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("docs-lint inputs must include apps/studio/docs/manifest.toml"),
        "{err:#}"
    );
}

#[test]
fn lint_docs_contract_requires_docs_lint_studio_docs_pages_input() {
    let mut config = docs_task_file(
        vec!["docs-contract"],
        vec!["docs-contract"],
        vec!["docs-lint"],
    );
    let docs_lint = config
        .tasks
        .iter_mut()
        .find(|task| task.name == "docs-lint")
        .expect("docs-lint task");
    docs_lint
        .inputs
        .retain(|input| input != "apps/studio/docs/pages/**");

    let err = lint_vm_production_selects_docs_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("docs-lint inputs must include apps/studio/docs/pages/**"),
        "{err:#}"
    );
}

#[test]
fn lint_docs_contract_accepts_group_ownership() {
    let config = docs_task_file(
        vec!["docs-contract"],
        vec!["docs-contract"],
        vec!["docs-lint"],
    );
    lint_vm_production_selects_docs_contract(&config).unwrap();
}

#[test]
fn lint_app_contract_requires_studio_wasm_source_contract_task_061() {
    let config = app_contract_task_file(
        vec!["app-contract"],
        vec!["app-contract"],
        vec!["app-contract"],
        vec!["wasm-check", "cargo-test-web-hardening", "vo-test-wasm"],
    );
    let err = lint_vm_production_selects_app_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}")
            .contains("app-contract must select cargo-test-studio-wasm-source-contract"),
        "{err:#}"
    );
}

#[test]
fn lint_app_contract_accepts_studio_wasm_source_contract_task_061() {
    let config = app_contract_task_file(
        vec!["app-contract"],
        vec!["app-contract"],
        vec!["app-contract"],
        vec![
            "wasm-check",
            "cargo-test-web-hardening",
            "cargo-test-studio-wasm-source-contract",
            "cargo-test-web-runtime-wasm",
            "vo-test-wasm",
        ],
    );
    lint_vm_production_selects_app_contract(&config).unwrap();
}

#[test]
fn lint_app_contract_requires_studio_wasm_source_contract_ts_input_062() {
    let mut config = app_contract_task_file(
        vec!["app-contract"],
        vec!["app-contract"],
        vec!["app-contract"],
        vec![
            "wasm-check",
            "cargo-test-web-hardening",
            "cargo-test-studio-wasm-source-contract",
            "cargo-test-web-runtime-wasm",
            "vo-test-wasm",
        ],
    );
    let task = config
        .tasks
        .iter_mut()
        .find(|task| task.name == "cargo-test-studio-wasm-source-contract")
        .expect("studio wasm source contract task");
    task.inputs
        .retain(|input| input != "apps/studio/src/lib/studio_wasm.ts");

    let err = lint_vm_production_selects_app_contract(&config).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "cargo-test-studio-wasm-source-contract inputs must include apps/studio/src/lib/studio_wasm.ts"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_app_contract_requires_web_runtime_wasm_source_contract_task_061() {
    let config = app_contract_task_file(
        vec!["app-contract"],
        vec!["app-contract"],
        vec!["app-contract"],
        vec![
            "wasm-check",
            "cargo-test-web-hardening",
            "cargo-test-studio-wasm-source-contract",
            "vo-test-wasm",
        ],
    );
    let err = lint_vm_production_selects_app_contract(&config).unwrap_err();
    assert!(
        format!("{err:#}").contains("app-contract must select cargo-test-web-runtime-wasm"),
        "{err:#}"
    );
}

#[test]
fn lint_selected_gate_tasks_have_timeouts_rejects_non_test_task_without_timeout() {
    let mut groups = BTreeMap::new();
    groups.insert("contract".to_string(), vec!["docs-lint".to_string()]);
    groups.insert("vm-production".to_string(), vec!["wasm-check".to_string()]);
    groups.insert("site".to_string(), vec!["site-build".to_string()]);
    groups.insert(
        "release-verify".to_string(),
        vec!["release-verify-vogui".to_string()],
    );
    let config = TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![
            task_without_timeout("docs-lint"),
            task("wasm-check"),
            task("site-build"),
            task("release-verify-vogui"),
        ],
    };
    let task_map = task_map(&config).unwrap();

    let err = lint_selected_gate_tasks_have_timeouts(&config, &task_map).unwrap_err();

    assert!(
        format!("{err:#}").contains("contract selected task docs-lint has no timeout_sec"),
        "{err:#}"
    );
}

#[test]
fn lint_selected_gate_tasks_have_timeouts_rejects_unbounded_final_signoff_selector_task_058() {
    let mut groups = BTreeMap::new();
    groups.insert("contract".to_string(), vec!["docs-lint".to_string()]);
    groups.insert("vm-production".to_string(), vec!["wasm-check".to_string()]);
    groups.insert("site".to_string(), vec!["site-build".to_string()]);
    groups.insert(
        "release-verify".to_string(),
        vec!["release-verify-vogui".to_string()],
    );
    let config = TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![
            task("docs-lint"),
            task("wasm-check"),
            task_without_timeout("site-build"),
            task("release-verify-vogui"),
        ],
    };
    let task_map = task_map(&config).unwrap();

    let err = lint_selected_gate_tasks_have_timeouts(&config, &task_map).unwrap_err();

    assert!(
        format!("{err:#}").contains("site selected task site-build has no timeout_sec"),
        "{err:#}"
    );
}

#[test]
fn lint_selected_gate_tasks_have_timeouts_accepts_bounded_gate_tasks() {
    let mut groups = BTreeMap::new();
    groups.insert("contract".to_string(), vec!["docs-lint".to_string()]);
    groups.insert("vm-production".to_string(), vec!["wasm-check".to_string()]);
    groups.insert("site".to_string(), vec!["site-build".to_string()]);
    groups.insert(
        "release-verify".to_string(),
        vec!["release-verify-vogui".to_string()],
    );
    let config = TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks: vec![
            task("docs-lint"),
            task("wasm-check"),
            task("site-build"),
            task("release-verify-vogui"),
        ],
    };
    let task_map = task_map(&config).unwrap();

    lint_selected_gate_tasks_have_timeouts(&config, &task_map).unwrap();
}

#[test]
fn lint_stdlib_embedded_source_inputs_requires_source_tree() {
    let task_map = BTreeMap::from([
        (
            "cargo-test-stdlib".to_string(),
            task_with_inputs("cargo-test-stdlib", &["lang/crates/vo-stdlib/**"]),
        ),
        (
            "cargo-test-web-runtime-wasm".to_string(),
            task_with_inputs(
                "cargo-test-web-runtime-wasm",
                &["lang/crates/vo-web/runtime-wasm/**", "lang/stdlib/**"],
            ),
        ),
    ]);

    let err = lint_stdlib_embedded_source_inputs(&task_map).unwrap_err();

    assert!(
        format!("{err:#}").contains("task cargo-test-stdlib must include input lang/stdlib/**"),
        "{err:#}"
    );
}

#[test]
fn lint_stdlib_embedded_source_inputs_accepts_embedded_source_tree() {
    let task_map = BTreeMap::from([
        (
            "cargo-test-stdlib".to_string(),
            task_with_inputs(
                "cargo-test-stdlib",
                &["lang/crates/vo-stdlib/**", "lang/stdlib/**"],
            ),
        ),
        (
            "cargo-test-web-runtime-wasm".to_string(),
            task_with_inputs(
                "cargo-test-web-runtime-wasm",
                &["lang/crates/vo-web/runtime-wasm/**", "lang/stdlib/**"],
            ),
        ),
    ]);

    lint_stdlib_embedded_source_inputs(&task_map).unwrap();
}

#[test]
fn lint_vo_dev_source_contract_consumer_inputs_requires_helper_crate_062() {
    let task_map = BTreeMap::from([(
        "cargo-test-vo-dev".to_string(),
        task_with_inputs("cargo-test-vo-dev", &["cmd/vo-dev/**"]),
    )]);

    let err = lint_vo_dev_source_contract_consumer_inputs(&task_map).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "task cargo-test-vo-dev must include input lang/crates/vo-source-contract/**"
        ),
        "{err:#}"
    );
}

#[test]
fn lint_vo_dev_source_contract_consumer_inputs_accepts_helper_crate_062() {
    let task_map = BTreeMap::from([(
        "cargo-test-vo-dev".to_string(),
        task_with_inputs(
            "cargo-test-vo-dev",
            &["cmd/vo-dev/**", "lang/crates/vo-source-contract/**"],
        ),
    )]);

    lint_vo_dev_source_contract_consumer_inputs(&task_map).unwrap();
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_stdlib_source_prefix() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "lang/stdlib/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix lang/stdlib/**"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_accepts_stdlib_contract_route() {
    let prefixes = vm_readiness_prefixes();

    lint_vm_readiness_changed_prefixes(&prefixes).unwrap();
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_codegen_osr_route_058() {
    let mut prefixes = vm_readiness_prefixes();
    let codegen = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "lang/crates/vo-codegen/**")
        .expect("codegen prefix");
    codegen.tasks.retain(|task| task != "vo-test-osr");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix lang/crates/vo-codegen/** must select vo-test-osr for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_lang_tests_route_058() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "tests/lang/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix tests/lang/**"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_lang_tests_full_matrix_route_058() {
    for required in [
        "vo-test-compile",
        "vo-test-osr",
        "vo-test-nostd",
        "vo-test-wasm",
        "vo-test-gc",
    ] {
        let mut prefixes = vm_readiness_prefixes();
        let tests_lang = prefixes
            .iter_mut()
            .find(|prefix| prefix.path == "tests/lang/**")
            .expect("tests/lang prefix");
        tests_lang.tasks.retain(|task| task != required);

        let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

        assert!(
                format!("{err:#}").contains(&format!(
                    "eng/ci.toml known_prefix tests/lang/** must select {required} for VM readiness changed-mode coverage"
                )),
                "{err:#}"
            );
    }
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_ffi_macro_route_055() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "lang/crates/vo-ffi-macro/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "eng/ci.toml missing VM readiness changed-mode prefix lang/crates/vo-ffi-macro/**"
        ),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_vo_ext_route_055() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "lang/crates/vo-ext/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix lang/crates/vo-ext/**"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_app_runtime_surface_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "lang/crates/vo-app-runtime/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "eng/ci.toml missing VM readiness changed-mode prefix lang/crates/vo-app-runtime/**"
        ),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_engine_surface_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "lang/crates/vo-engine/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "eng/ci.toml missing VM readiness changed-mode prefix lang/crates/vo-engine/**"
        ),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_vo_web_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "lang/crates/vo-web/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix lang/crates/vo-web/**"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_vo_web_hardening_route() {
    let mut prefixes = vm_readiness_prefixes();
    let vo_web_prefix = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "lang/crates/vo-web/**")
        .expect("vo-web route");
    vo_web_prefix
        .tasks
        .retain(|task| task != "cargo-test-web-hardening");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix lang/crates/vo-web/** must select cargo-test-web-hardening for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_web_runtime_wasm_contract_route_061() {
    let mut prefixes = vm_readiness_prefixes();
    let vo_web_prefix = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "lang/crates/vo-web/**")
        .expect("vo-web route");
    vo_web_prefix
        .tasks
        .retain(|task| task != "cargo-test-web-runtime-wasm");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix lang/crates/vo-web/** must select cargo-test-web-runtime-wasm for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_playground_host_wake_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "apps/playground-legacy/rust/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "eng/ci.toml missing VM readiness changed-mode prefix apps/playground-legacy/rust/**"
        ),
        "{err:#}"
    );
}

#[test]
fn lint_playground_host_wake_task_filter_requires_host_wake_prefix() {
    let task_map = BTreeMap::from([(
        "cargo-test-vo-playground-host-wake".to_string(),
        task("cargo-test-vo-playground-host-wake"),
    )]);

    let err = lint_playground_host_wake_task_filter(&task_map).unwrap_err();

    assert!(
        format!("{err:#}").contains("cargo-test-vo-playground-host-wake must use host_wake filter"),
        "{err:#}"
    );
}

#[test]
fn lint_playground_host_wake_task_filter_accepts_host_wake_prefix() {
    let mut host_wake_task = task("cargo-test-vo-playground-host-wake");
    host_wake_task.command.push("host_wake".to_string());
    let task_map = BTreeMap::from([(
        "cargo-test-vo-playground-host-wake".to_string(),
        host_wake_task,
    )]);

    lint_playground_host_wake_task_filter(&task_map).unwrap();
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_studio_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "apps/studio/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix apps/studio/**"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_studio_docs_lint_route() {
    let mut prefixes = vm_readiness_prefixes();
    let studio_prefix = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "apps/studio/**")
        .expect("studio route");
    studio_prefix.tasks.retain(|task| task != "docs-lint");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix apps/studio/** must select docs-lint for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_studio_build_route() {
    let mut prefixes = vm_readiness_prefixes();
    let studio_prefix = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "apps/studio/**")
        .expect("studio route");
    studio_prefix.tasks.retain(|task| task != "studio-build");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix apps/studio/** must select studio-build for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_studio_wasm_source_contract_route_061() {
    let mut prefixes = vm_readiness_prefixes();
    let studio_prefix = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "apps/studio/**")
        .expect("studio route");
    studio_prefix
        .tasks
        .retain(|task| task != "cargo-test-studio-wasm-source-contract");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix apps/studio/** must select cargo-test-studio-wasm-source-contract for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_vo_dev_contract_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "cmd/vo-dev/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix cmd/vo-dev/**"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_runtime_vm_hardening_route() {
    let mut prefixes = vm_readiness_prefixes();
    let runtime = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "lang/crates/vo-runtime/**")
        .expect("runtime prefix fixture");
    runtime
        .tasks
        .retain(|task| task != "cargo-test-vm-hardening");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix lang/crates/vo-runtime/** must select cargo-test-vm-hardening"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_source_contract_route_062() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "lang/crates/vo-source-contract/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml missing VM readiness changed-mode prefix lang/crates/vo-source-contract/**"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_source_contract_task_062() {
    let mut prefixes = vm_readiness_prefixes();
    let source_contract = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "lang/crates/vo-source-contract/**")
        .expect("source-contract route");
    source_contract
        .tasks
        .retain(|task| task != "cargo-test-vo-source-contract");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix lang/crates/vo-source-contract/** must select cargo-test-vo-source-contract for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_source_contract_consumers_062() {
    for required in [
        "cargo-test-runtime",
        "cargo-test-vo-dev",
        "cargo-test-jit-hardening",
        "cargo-test-vm-hardening",
        "cargo-test-vm-hardening-jit",
    ] {
        let mut prefixes = vm_readiness_prefixes();
        let source_contract = prefixes
            .iter_mut()
            .find(|prefix| prefix.path == "lang/crates/vo-source-contract/**")
            .expect("source-contract route");
        source_contract.tasks.retain(|task| task != required);

        let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

        assert!(
                format!("{err:#}").contains(&format!(
                    "eng/ci.toml known_prefix lang/crates/vo-source-contract/** must select {required} for VM readiness changed-mode coverage"
                )),
                "{err:#}"
            );
    }
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_analysis_compile_route_060() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "lang/crates/vo-analysis/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "eng/ci.toml missing VM readiness changed-mode prefix lang/crates/vo-analysis/**"
        ),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_codegen_compile_route_060() {
    let mut prefixes = vm_readiness_prefixes();
    let codegen = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "lang/crates/vo-codegen/**")
        .expect("codegen prefix fixture");
    codegen.tasks.retain(|task| task != "vo-test-compile");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "eng/ci.toml known_prefix lang/crates/vo-codegen/** must select vo-test-compile"
        ),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_eng_task_manifest_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "eng/tasks.toml")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix eng/tasks.toml"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_eng_ci_manifest_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "eng/ci.toml")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix eng/ci.toml"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_production_readiness_workflow_route() {
    let prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != ".github/workflows/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix .github/workflows/**"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_workflow_self_check_route_058() {
    let mut prefixes = vm_readiness_prefixes();
    let workflow_prefix = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == ".github/workflows/**")
        .expect("workflow prefix");
    workflow_prefix.tasks.retain(|task| task != "ci-self-check");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix .github/workflows/** must select ci-self-check for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_workflow_docs_lint_route_059() {
    let mut prefixes = vm_readiness_prefixes();
    let workflow_prefix = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == ".github/workflows/**")
        .expect("workflow prefix");
    workflow_prefix.tasks.retain(|task| task != "docs-lint");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();

    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix .github/workflows/** must select docs-lint for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_pr_scope_for_required_tasks_059() {
    let prefixes = vm_readiness_prefixes();
    let config = vm_readiness_scope_task_file("pr", Some("vo-test-gc"));

    let err = lint_vm_readiness_changed_prefix_scopes(&prefixes, &config).unwrap_err();

    assert!(
            format!("{err:#}")
                .contains("eng/tasks.toml pr scope must include vo-test-gc selected by VM readiness changed-mode prefix"),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_vm_production_compile_scope_060() {
    let prefixes = vm_readiness_prefixes();
    let config = vm_readiness_scope_task_file("vm-production", Some("vo-test-compile"));

    let err = lint_vm_readiness_changed_prefix_scopes(&prefixes, &config).unwrap_err();

    assert!(
            format!("{err:#}")
                .contains("eng/tasks.toml vm-production scope must include vo-test-compile selected by VM readiness changed-mode prefix"),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_vm_production_analysis_scope_062() {
    let prefixes = vm_readiness_prefixes();
    let config = vm_readiness_scope_task_file("vm-production", Some("cargo-test-analysis"));

    let err = lint_vm_readiness_changed_prefix_scopes(&prefixes, &config).unwrap_err();

    assert!(
            format!("{err:#}").contains("eng/tasks.toml vm-production scope must include cargo-test-analysis selected by VM readiness changed-mode prefix lang/crates/vo-analysis/**"),
            "{err:#}"
        );
}

#[test]
fn lint_vm_readiness_changed_prefixes_requires_docs_lint_script_route_058() {
    let mut prefixes: Vec<_> = vm_readiness_prefixes()
        .into_iter()
        .filter(|prefix| prefix.path != "scripts/ci/**")
        .collect();

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();
    assert!(
        format!("{err:#}")
            .contains("eng/ci.toml missing VM readiness changed-mode prefix scripts/ci/**"),
        "{err:#}"
    );

    prefixes.push(known_prefix("scripts/ci/**", &["ci-self-check"]));
    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();
    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix scripts/ci/** must select eng-lint-tasks for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );

    let mut prefixes = vm_readiness_prefixes();
    let scripts_ci = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "scripts/ci/**")
        .expect("scripts/ci route");
    scripts_ci.tasks.retain(|task| task != "docs-lint");
    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();
    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix scripts/ci/** must select docs-lint for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );

    let mut prefixes = vm_readiness_prefixes();
    let scripts_ci = prefixes
        .iter_mut()
        .find(|prefix| prefix.path == "scripts/ci/**")
        .expect("scripts/ci route");
    scripts_ci.tasks.retain(|task| task != "quickplay-validate");

    let err = lint_vm_readiness_changed_prefixes(&prefixes).unwrap_err();
    assert!(
            format!("{err:#}").contains(
                "eng/ci.toml known_prefix scripts/ci/** must select quickplay-validate for VM readiness changed-mode coverage"
            ),
            "{err:#}"
        );
}

#[test]
fn lint_vm_hardening_tasks_accepts_unfiltered_crate_tests_059() {
    let task_map = vm_hardening_task_map();

    lint_vm_hardening_tasks_run_unfiltered_crate_tests(&task_map).unwrap();
}

#[test]
fn lint_vm_hardening_tasks_rejects_name_filtered_crate_tests_059() {
    let mut task_map = vm_hardening_task_map();
    task_map
        .get_mut("cargo-test-vm-hardening")
        .expect("vm hardening task")
        .command
        .push("vm_".to_string());

    let err = lint_vm_hardening_tasks_run_unfiltered_crate_tests(&task_map).unwrap_err();

    assert!(
        format!("{err:#}").contains("must run unfiltered crate tests"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_jit_manager_surface_rejects_public_jit_state() {
    let err = lint_vm_jit_manager_surface_in_source(
        r#"
            pub mod jit_mgr;
            pub use jit_mgr::{JitConfig, JitManager};

            pub enum VmJitState {
                Disabled,
                Strict(JitManager),
            }

            pub struct Vm {
                pub jit: VmJitState,
            }

            impl Vm {
                pub fn replace_extern_registry_for_testing(&mut self) {}
            }
            "#,
    )
    .unwrap_err();

    assert!(
        format!("{err:#}").contains("jit_mgr must remain a private VM module"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_jit_manager_surface_accepts_vm_owned_state() {
    lint_vm_jit_manager_surface_in_source(
        r#"
            mod jit_mgr;
            pub use jit_mgr::JitConfig;
            use jit_mgr::JitManager;

            enum VmJitState {
                Disabled,
                Strict(JitManager),
            }

            pub struct Vm {
                jit: VmJitState,
            }
            "#,
    )
    .unwrap();
}

fn voplay_gate_source_fixture() -> (String, String, String, String) {
    let readiness = [
        "const sourceFactRequirements = [];",
        "const evidenceTable = [];",
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
    ]
    .into_iter()
    .chain(VOPLAY_REQUIRED_SOURCE_FACTS.iter().copied())
    .collect::<Vec<_>>()
    .join("\n");
    let render_stress = [
        "'render.perf_gate_failed'",
        "'render.p90_over_budget'",
        "'render.p99_over_budget'",
        "'render.slow_frames_over_budget'",
        "'summary.p90_over_budget'",
        "'summary.p99_over_budget'",
        "'summary.slow_frames_over_budget'",
        "p1 += summaryIssues.filter((issue) => issue.severity === 1).length",
        "status: p0 === 0 && p1 === 0 ? 'pass' : 'fail'",
        "if (report.status !== 'pass')",
    ]
    .join("\n");
    let architecture = VOPLAY_RENDER_ARCHITECTURE_FAILURE_CODES
        .iter()
        .copied()
        .chain(
            [
                "constructsRuntimeStage(rendererAuditSource, token)",
                "execute_render_node!",
                "SurfaceMaterialAtTrackPosition",
                "Body\\.SetPosition",
                "applyPoseResetToBackend",
                "ApplyVehicleForces",
                "PrimitiveStats",
                "primitive3d\\.NewBuilder",
                "w\\.player\\.SetVelocity",
            ]
            .into_iter(),
        )
        .collect::<Vec<_>>()
        .join("\n");
    let blockkart_boundary = VOPLAY_BLOCKKART_BOUNDARY_FAILURE_CODES
        .iter()
        .copied()
        .chain(
            [
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
            ]
            .into_iter(),
        )
        .collect::<Vec<_>>()
        .join("\n");
    (readiness, render_stress, architecture, blockkart_boundary)
}

fn voplay_gate_task_file(site_items: &[&str], eng_lint_inputs: &[&str]) -> TaskFile {
    let app_site_tasks = [
        "voplay-render-architecture-lint",
        "blockkart-engine-boundary-lint",
        "voplay-render-stress-budgeted",
        "voplay-render-soak-10m",
        "voplay-physics-industrial-stress",
    ];
    let industrial_tasks = [
        "voplay-industrial-source-audit",
        "voplay-industrial-readiness-report",
        "voplay-industrial-readiness",
    ];
    let mut groups = BTreeMap::new();
    groups.insert(
        "app-site".to_string(),
        app_site_tasks
            .iter()
            .map(|item| (*item).to_string())
            .collect(),
    );
    groups.insert(
        "voplay-industrial".to_string(),
        industrial_tasks
            .iter()
            .map(|item| (*item).to_string())
            .collect(),
    );
    groups.insert(
        "site".to_string(),
        site_items.iter().map(|item| (*item).to_string()).collect(),
    );
    let mut tasks = app_site_tasks
        .iter()
        .chain(industrial_tasks.iter())
        .map(|name| task(name))
        .collect::<Vec<_>>();
    tasks.push(task_with_inputs("eng-lint-tasks", eng_lint_inputs));
    TaskFile {
        version: 1,
        final_selectors: final_selectors(),
        groups,
        group_meta: vec![],
        tasks,
    }
}

#[test]
fn lint_voplay_industrial_gate_source_policy_accepts_required_sentinels() {
    let (readiness, render_stress, architecture, blockkart_boundary) = voplay_gate_source_fixture();

    lint_voplay_industrial_gate_sources(
        &readiness,
        &render_stress,
        &architecture,
        &blockkart_boundary,
    )
    .unwrap();
}

#[test]
fn lint_voplay_industrial_gate_source_policy_rejects_missing_required_fact() {
    let (readiness, render_stress, architecture, blockkart_boundary) = voplay_gate_source_fixture();
    let readiness = readiness.replace("batch_plan_real_bounds", "");

    let err = lint_voplay_industrial_gate_sources(
        &readiness,
        &render_stress,
        &architecture,
        &blockkart_boundary,
    )
    .unwrap_err();

    assert!(
        format!("{err:#}").contains("batch_plan_real_bounds"),
        "{err:#}"
    );
}

#[test]
fn lint_voplay_industrial_gate_source_policy_rejects_host_pacing_budget_bypass() {
    let (readiness, mut render_stress, architecture, blockkart_boundary) =
        voplay_gate_source_fixture();
    render_stress.push_str("\nif (hostPacingOnly) return diagnosticsOnly;\n");

    let err = lint_voplay_industrial_gate_sources(
        &readiness,
        &render_stress,
        &architecture,
        &blockkart_boundary,
    )
    .unwrap_err();

    assert!(format!("{err:#}").contains("hostPacingOnly"), "{err:#}");
}

#[test]
fn lint_voplay_industrial_gate_task_wiring_requires_site_final_gate() {
    let config = voplay_gate_task_file(&["app-site"], VOPLAY_INDUSTRIAL_GATE_SCRIPT_INPUTS);
    let task_map = task_map(&config).unwrap();

    let err = lint_voplay_industrial_gate_task_wiring(&config, &task_map).unwrap_err();

    assert!(
        format!("{err:#}").contains("site scope must include voplay-industrial-source-audit"),
        "{err:#}"
    );
}

#[test]
fn lint_voplay_industrial_gate_task_wiring_requires_eng_lint_script_inputs() {
    let inputs = VOPLAY_INDUSTRIAL_GATE_SCRIPT_INPUTS
        .iter()
        .copied()
        .filter(|input| *input != "scripts/ci/voplay_render_stress.mjs")
        .collect::<Vec<_>>();
    let config = voplay_gate_task_file(&["app-site", "voplay-industrial"], &inputs);
    let task_map = task_map(&config).unwrap();

    let err = lint_voplay_industrial_gate_task_wiring(&config, &task_map).unwrap_err();

    assert!(
        format!("{err:#}").contains("scripts/ci/voplay_render_stress.mjs"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_hardening_uses_unfiltered_tasks_not_test_name_prefixes_060() {
    let source = include_str!("../lint_system.rs")
        .split("\n#[cfg(test)]\nmod tests")
        .next()
        .expect("lint source should contain test module");

    assert!(
        source.contains("lint_vm_hardening_tasks_run_unfiltered_crate_tests"),
        "VM hardening coverage must be owned by unfiltered crate-task contracts"
    );
    assert!(
        !source.contains("must start with vm_"),
        "VM hardening lint must not rely on stale test-name prefixes after unfiltered crate tasks"
    );
}

#[test]
fn lint_vm_hardening_selection_062_has_semantic_gate() {
    let source = include_str!("../lint_system.rs")
        .split("\n#[cfg(test)]\nmod tests")
        .next()
        .expect("lint source should contain test module");

    assert!(
        source.contains("lint_vm_production_selects_vm_hardening_contract"),
        "VM hardening must have a semantic selector lint so final gates cannot drop blocker proofs"
    );
}

#[test]
fn lint_vm_hardening_requires_contract_gate_062() {
    let config = vm_hardening_task_file(
        vec![],
        vec!["vm-hardening"],
        vec!["vm-hardening"],
        VM_HARDENING_UNFILTERED_CRATE_TESTS
            .iter()
            .map(|(name, _)| *name)
            .collect(),
    );

    let err = lint_vm_production_selects_vm_hardening_contract(&config).unwrap_err();

    assert!(
        format!("{err:#}").contains("contract must include vm-hardening"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_hardening_requires_vm_production_gate_062() {
    let config = vm_hardening_task_file(
        vec!["vm-hardening"],
        vec![],
        vec!["vm-hardening"],
        VM_HARDENING_UNFILTERED_CRATE_TESTS
            .iter()
            .map(|(name, _)| *name)
            .collect(),
    );

    let err = lint_vm_production_selects_vm_hardening_contract(&config).unwrap_err();

    assert!(
        format!("{err:#}").contains("vm-production must include vm-hardening"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_hardening_requires_pr_gate_062() {
    let config = vm_hardening_task_file(
        vec!["vm-hardening"],
        vec!["vm-hardening"],
        vec![],
        VM_HARDENING_UNFILTERED_CRATE_TESTS
            .iter()
            .map(|(name, _)| *name)
            .collect(),
    );

    let err = lint_vm_production_selects_vm_hardening_contract(&config).unwrap_err();

    assert!(
        format!("{err:#}").contains("pr must include vm-hardening"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_hardening_requires_all_hardening_tasks_062() {
    let mut hardening_tasks: Vec<_> = VM_HARDENING_UNFILTERED_CRATE_TESTS
        .iter()
        .map(|(name, _)| *name)
        .collect();
    hardening_tasks.retain(|task| *task != "cargo-test-vm-hardening-jit");
    let config = vm_hardening_task_file(
        vec!["vm-hardening"],
        vec!["vm-hardening"],
        vec!["vm-hardening"],
        hardening_tasks,
    );

    let err = lint_vm_production_selects_vm_hardening_contract(&config).unwrap_err();

    assert!(
        format!("{err:#}").contains("vm-hardening must select cargo-test-vm-hardening-jit"),
        "{err:#}"
    );
}

#[test]
fn lint_vm_hardening_accepts_gate_owned_hardening_selector_062() {
    let config = vm_hardening_task_file(
        vec!["vm-hardening"],
        vec!["vm-hardening"],
        vec!["vm-hardening"],
        VM_HARDENING_UNFILTERED_CRATE_TESTS
            .iter()
            .map(|(name, _)| *name)
            .collect(),
    );

    lint_vm_production_selects_vm_hardening_contract(&config).unwrap();
}

#[test]
fn studio_wasm_source_contract_061_validates_serialized_module_cache_before_reuse() {
    let source = include_str!("../../../../apps/studio/wasm/src/lib.rs");
    assert_studio_wasm_verified_decoder_contract(source).unwrap();
}

#[test]
fn studio_wasm_source_contract_061_rejects_raw_deserialize_bypass() {
    let source = include_str!("../../../../apps/studio/wasm/src/lib.rs").replace(
        "decode_verified_module(&bytecode, \"Studio compile cache\").is_err()",
        "vo_vm::bytecode::Module::deserialize(&bytecode).is_err()",
    );
    let err = assert_studio_wasm_verified_decoder_contract(&source).unwrap_err();

    assert!(
        format!("{err:#}").contains("centralized in decode_verified_module"),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_062_validates_pending_host_event_adapter_fields() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs");
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    assert_studio_wasm_pending_host_event_contract(rust_source, ts_source).unwrap();
}

#[test]
fn studio_wasm_source_contract_062_rejects_key_derived_host_event_source() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs").replace(
            "&JsValue::from_str(event.source.as_str())",
            "&JsValue::from_str(&event.key.encode())\n        // &JsValue::from_str(event.source.as_str())",
        );
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    let err = assert_studio_wasm_pending_host_event_contract(&rust_source, ts_source).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "pending host event serializer must emit source from the VM-owned event field"
        ),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_062_rejects_hardcoded_host_event_replay() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs").replace(
        "&JsValue::from_bool(event.replay)",
        "&JsValue::from_bool(false)\n        // &JsValue::from_bool(event.replay)",
    );
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    let err = assert_studio_wasm_pending_host_event_contract(&rust_source, ts_source).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "pending host event serializer must emit replay from the VM-owned event field"
        ),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_062_rejects_split_legacy_host_event_adapter() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs").replace(
        "pending_host_event_to_js(&event).into()",
        r#"{
            // pending_host_event_to_js(&event).into()
            let obj = Object::new();
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("source"),
                &JsValue::from_str(event.key.source.as_str()),
            );
            obj.into()
        }"#,
    );
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    let err = assert_studio_wasm_pending_host_event_contract(&rust_source, ts_source).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("pollPendingHostEvent must share pending host event serialization"),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_062_rejects_legacy_ts_without_replay() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs");
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts").replace(
            "pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number; replay: boolean } | null;",
            "// pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number; replay: boolean } | null;\n  pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number } | null;",
        );
    let err = assert_studio_wasm_pending_host_event_contract(rust_source, &ts_source).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "Studio wasm TypeScript facade must expose legacy source and replay host event fields"
        ),
        "{err:#}"
    );
}
