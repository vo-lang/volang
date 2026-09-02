use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Component, Path};

const REQUIRED_PROTOCOLS: [&str; 9] = [
    "VUP1", "VUA1", "VUB1", "VUI1", "VUE1", "VUS1", "VAX1", "VPX1", "VWX1",
];
const CAPABILITY_MATURITY: [&str; 6] = [
    "specified",
    "implemented",
    "conformant",
    "dogfooded",
    "hardened",
    "stable",
];
const API_STABILITY: [&str; 5] = [
    "internal",
    "experimental",
    "preview",
    "stable",
    "deprecated",
];
const CAPABILITY_TIERS: [&str; 6] = [
    "core",
    "standard",
    "extended",
    "platform",
    "tooling",
    "ecosystem",
];
const UI_TARGETS: [&str; 7] = [
    "headless",
    "web-vm",
    "web-wasm-aot",
    "desktop-vm",
    "desktop-jit",
    "desktop-native-aot",
    "server-native-aot",
];
const UIKIT_TARGETS: [&str; 6] = [
    "headless",
    "web-vm",
    "web-wasm-aot",
    "desktop-vm",
    "desktop-jit",
    "desktop-native-aot",
];
const DELIVERY_INCREMENTS: [&str; 9] = ["E0", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8"];
const CONTRACT_PROBES: [&str; 6] = [
    "nested-component-list",
    "asynchronous-search",
    "modal-validation-form",
    "virtual-data-grid",
    "ssr-activation-route",
    "multi-window-editor",
];
const MODULE_PROFILE_CAPABILITIES: [&str; 6] =
    ["core", "graphics", "media", "editor", "platform", "testing"];
const MODULE_PROFILES: [&str; 5] = ["minimal", "application", "web", "studio", "full"];
const QUALITY_SUITES: [&str; 7] = [
    "portable-contracts",
    "web-real-browser",
    "native-real-platform",
    "advanced-packs",
    "resilience-security",
    "performance",
    "release-artifacts",
];
const QUALITY_TARGETS: [&str; 10] = [
    "headless",
    "web-vm",
    "web-wasm-aot",
    "desktop-vm",
    "desktop-jit",
    "desktop-native-aot",
    "server-native-aot",
    "linux",
    "macos",
    "windows",
];
const STUDIO_TARGETS: [&str; 2] = ["web", "desktop"];
const STUDIO_CAPABILITIES: [&str; 20] = [
    "home",
    "starter-gallery",
    "starter-edit-run",
    "project-create",
    "project-open-import-recent",
    "project-search",
    "project-rename-delete",
    "project-share",
    "github-account",
    "remote-status",
    "remote-diff-conflicts",
    "remote-pull-push",
    "remote-delete-authority",
    "workbench",
    "responsive-workbench",
    "diagnostic-navigation",
    "command-palette",
    "independent-runner",
    "documentation-center",
    "release-backends",
];
const PRODUCT_DOCUMENTS: [&str; 8] = [
    "ui/docs/getting-started.md",
    "ui/docs/authoring-guide.md",
    "ui/docs/testing-troubleshooting.md",
    "ui/docs/accessibility-localization.md",
    "ui/docs/security.md",
    "ui/docs/compatibility-migration.md",
    "ui/docs/contributing-support.md",
    "ui/docs/release-notes-1.0.md",
];
const GENERATED_EVIDENCE_PREFIX: &str = "generated:";
const GENERATED_EVIDENCE_ROOT: &str = "target/rewrite-validation/";

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct CertificationFile {
    schema_version: u32,
    product: String,
    framework_version: String,
    certification_level: String,
    roadmap: String,
    product_roadmap: String,
    protocols: Vec<String>,
    gate: Vec<CertificationGate>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct CertificationGate {
    id: String,
    summary: String,
    commands: Vec<String>,
    evidence: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RoadmapFile {
    scope: String,
    completion_claim: String,
    frozen: bool,
    product_status: String,
    product_roadmap: String,
    capability_catalog: String,
    delivery_plan: String,
    completion: RoadmapCompletion,
    milestone: Vec<RoadmapMilestone>,
    #[serde(default)]
    work_item: Vec<RoadmapWorkItem>,
}

#[derive(Debug, Deserialize)]
struct RoadmapCompletion {
    required_milestones: Vec<String>,
    required_gates: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RoadmapMilestone {
    id: String,
    status: String,
}

#[derive(Debug, Deserialize)]
struct RoadmapWorkItem {
    id: String,
    status: String,
    #[serde(default)]
    remaining: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProductRoadmapFile {
    schema_version: u32,
    product: String,
    target_version: String,
    status: String,
    baseline_roadmap: String,
    capability_catalog: String,
    delivery_plan: String,
    uikit_catalog: String,
    product_principle: String,
    benchmark_families: Vec<String>,
    completion: ProductCompletion,
    domain: Vec<ProductDomain>,
    showcase: Vec<ProductShowcase>,
    gate: Vec<ProductGate>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct UIKitCatalogFile {
    schema_version: u32,
    product: String,
    delivery: String,
    status: String,
    market_baseline: UIKitMarketBaseline,
    families: Vec<String>,
    targets: Vec<String>,
    themes: Vec<String>,
    densities: Vec<String>,
    directions: Vec<String>,
    component: Vec<UIKitComponent>,
    parity_gap: Vec<UIKitParityGap>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct UIKitMarketBaseline {
    status: String,
    benchmarks: Vec<String>,
    components: Vec<String>,
    quality_dimensions: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct UIKitParityGap {
    id: String,
    priority: String,
    benchmarks: Vec<String>,
    acceptance: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct UIKitComponent {
    id: String,
    family: String,
    status: String,
    api: String,
    behavior: String,
    variants: Vec<String>,
    states: Vec<String>,
    keyboard: Vec<String>,
    semantics: Vec<String>,
    #[serde(default)]
    evidence: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProductCompletion {
    required_capability_maturity: String,
    required_delivery_increment: String,
    required_domains: Vec<String>,
    required_showcases: Vec<String>,
    required_gates: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProductDomain {
    id: String,
    name: String,
    benchmarks: Vec<String>,
    outcome: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProductShowcase {
    id: String,
    name: String,
    status: String,
    starts_in: String,
    acceptance: Vec<String>,
    #[serde(default)]
    commands: Vec<String>,
    #[serde(default)]
    evidence: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProductGate {
    id: String,
    status: String,
    acceptance: Vec<String>,
    #[serde(default)]
    commands: Vec<String>,
    #[serde(default)]
    evidence: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProductCertificationFile {
    schema_version: u32,
    product: String,
    target_version: String,
    status: String,
    candidate_identity: String,
    evidence_schema: String,
    quality_matrix: String,
    release_workflow: String,
    required_gates: Vec<String>,
    required_showcases: Vec<String>,
    required_artifacts: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct CapabilityCatalogFile {
    schema_version: u32,
    product: String,
    target_version: String,
    maturity_order: Vec<String>,
    api_stability_order: Vec<String>,
    tiers: Vec<String>,
    targets: Vec<String>,
    capability: Vec<ProductCapability>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProductCapability {
    id: String,
    domain: String,
    name: String,
    delivery: String,
    status: String,
    api_stability: String,
    tier: String,
    required_for_1_0: bool,
    owners: Vec<String>,
    required_targets: Vec<String>,
    #[serde(default)]
    optional_targets: Vec<String>,
    depends_on: Vec<String>,
    acceptance: Vec<String>,
    #[serde(default)]
    evidence: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct DeliveryPlanFile {
    schema_version: u32,
    product: String,
    target_version: String,
    active_increment: String,
    status: String,
    stream: Vec<DeliveryStream>,
    increment: Vec<DeliveryIncrement>,
    contract_probe: Vec<ContractProbe>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct DeliveryStream {
    id: String,
    name: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct DeliveryIncrement {
    id: String,
    name: String,
    status: String,
    depends_on: Vec<String>,
    streams: Vec<String>,
    outcomes: Vec<String>,
    exit: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ContractProbe {
    id: String,
    name: String,
    status: String,
    increment: String,
    capabilities: Vec<String>,
    targets: Vec<String>,
    acceptance: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ModuleProfilesFile {
    schema_version: u32,
    module: String,
    default_profile: String,
    capability: Vec<ModuleProfileCapability>,
    profile: Vec<ModuleProfile>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ModuleProfileCapability {
    id: String,
    requires: Vec<String>,
    packages: Vec<String>,
    targets: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ModuleProfile {
    id: String,
    capabilities: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct QualityMatrixFile {
    schema_version: u32,
    product: String,
    suite: Vec<QualitySuite>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct QualitySuite {
    id: String,
    targets: Vec<String>,
    commands: Vec<String>,
    evidence: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct StudioParityFile {
    schema: String,
    baseline: String,
    capability: Vec<StudioCapability>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct StudioCapability {
    id: String,
    status: String,
    targets: Vec<String>,
    acceptance: String,
    evidence: Vec<String>,
}

#[derive(Debug, Serialize)]
struct CertificationSummary<'a> {
    schema_version: u32,
    product: &'a str,
    framework_version: &'a str,
    milestone_count: usize,
    gate_count: usize,
    protocol_count: usize,
    product_target_version: &'a str,
    product_status: &'a str,
    product_domain_count: usize,
    product_capability_count: usize,
    stable_capability_count: usize,
    product_showcase_count: usize,
    product_gate_count: usize,
    active_increment: &'a str,
    contract_probe_count: usize,
    uikit_market_status: &'a str,
    uikit_component_count: usize,
    uikit_parity_gap_count: usize,
    studio_capability_count: usize,
    status: &'static str,
}

pub(crate) fn cmd_ui_certify(root: &Path, args: Vec<String>) -> Result<()> {
    let (json, require_generated_evidence) = match args.as_slice() {
        [] => (false, false),
        [arg] if arg == "--check" => (false, false),
        [arg] if arg == "--json" => (true, false),
        [arg] if arg == "--evidence" => (false, true),
        _ => bail!("usage: vo-dev ui-certify --check|--json|--evidence"),
    };
    let (certification, roadmap, product_roadmap, capabilities, delivery, studio) =
        load_and_validate(root)?;
    let verified_evidence = if require_generated_evidence {
        validate_generated_evidence_artifacts(root)?
    } else {
        0
    };
    let uikit = load_uikit_catalog(root, &product_roadmap)?;
    let stable_capability_count = capabilities
        .capability
        .iter()
        .filter(|capability| capability.status == "stable")
        .count();
    let summary = CertificationSummary {
        schema_version: certification.schema_version,
        product: &certification.product,
        framework_version: &certification.framework_version,
        milestone_count: roadmap.milestone.len(),
        gate_count: certification.gate.len(),
        protocol_count: certification.protocols.len(),
        product_target_version: &product_roadmap.target_version,
        product_status: &product_roadmap.status,
        product_domain_count: product_roadmap.domain.len(),
        product_capability_count: capabilities.capability.len(),
        stable_capability_count,
        product_showcase_count: product_roadmap.showcase.len(),
        product_gate_count: product_roadmap.gate.len(),
        active_increment: &delivery.active_increment,
        contract_probe_count: delivery.contract_probe.len(),
        uikit_market_status: &uikit.market_baseline.status,
        uikit_component_count: uikit.component.len(),
        uikit_parity_gap_count: uikit.parity_gap.len(),
        studio_capability_count: studio.capability.len(),
        status: if product_roadmap.status == "complete"
            && uikit.market_baseline.status == "complete"
        {
            "product-certified"
        } else {
            "foundation-certified"
        },
    };
    if json {
        println!("{}", serde_json::to_string(&summary)?);
    } else {
        println!(
            "{} {}: {} ({} milestones, {} foundation gates, {} protocols); product {} {} ({} domains, {} capabilities, {} stable, active {}, {} contract probes, {} showcases, {} product gates); UIKit market parity {} ({} implemented, {} governed gaps); Studio parity {} capabilities",
            summary.product,
            summary.framework_version,
            summary.status,
            summary.milestone_count,
            summary.gate_count,
            summary.protocol_count,
            summary.product_target_version,
            summary.product_status,
            summary.product_domain_count,
            summary.product_capability_count,
            summary.stable_capability_count,
            summary.active_increment,
            summary.contract_probe_count,
            summary.product_showcase_count,
            summary.product_gate_count,
            summary.uikit_market_status,
            summary.uikit_component_count,
            summary.uikit_parity_gap_count,
            summary.studio_capability_count,
        );
        if require_generated_evidence {
            println!("verified {verified_evidence} generated evidence reports");
        }
    }
    Ok(())
}

pub(crate) fn certification_status(root: &Path) -> Result<&'static str> {
    let (_, _, product_roadmap, _, _, _) = load_and_validate(root)?;
    let uikit = load_uikit_catalog(root, &product_roadmap)?;
    Ok(
        if product_roadmap.status == "complete" && uikit.market_baseline.status == "complete" {
            "product-certified"
        } else {
            "foundation-certified"
        },
    )
}

fn load_uikit_catalog(root: &Path, roadmap: &ProductRoadmapFile) -> Result<UIKitCatalogFile> {
    let path = checked_repo_path(root, &roadmap.uikit_catalog, "UIKit catalog")?;
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))
}

fn load_and_validate(
    root: &Path,
) -> Result<(
    CertificationFile,
    RoadmapFile,
    ProductRoadmapFile,
    CapabilityCatalogFile,
    DeliveryPlanFile,
    StudioParityFile,
)> {
    let certification_path = root.join("ui/certification.toml");
    let certification_text = fs::read_to_string(&certification_path)
        .with_context(|| format!("could not read {}", certification_path.display()))?;
    let certification: CertificationFile = toml::from_str(&certification_text)
        .with_context(|| format!("could not parse {}", certification_path.display()))?;
    validate_certification(root, &certification)?;

    let roadmap_path = checked_repo_path(root, &certification.roadmap, "roadmap")?;
    let roadmap_text = fs::read_to_string(&roadmap_path)
        .with_context(|| format!("could not read {}", roadmap_path.display()))?;
    let roadmap: RoadmapFile = toml::from_str(&roadmap_text)
        .with_context(|| format!("could not parse {}", roadmap_path.display()))?;
    validate_roadmap(&certification, &roadmap)?;

    let product_roadmap_path =
        checked_repo_path(root, &certification.product_roadmap, "product roadmap")?;
    let product_roadmap_text = fs::read_to_string(&product_roadmap_path)
        .with_context(|| format!("could not read {}", product_roadmap_path.display()))?;
    let product_roadmap: ProductRoadmapFile = toml::from_str(&product_roadmap_text)
        .with_context(|| format!("could not parse {}", product_roadmap_path.display()))?;
    let capability_path = checked_repo_path(
        root,
        &product_roadmap.capability_catalog,
        "capability catalog",
    )?;
    let capability_text = fs::read_to_string(&capability_path)
        .with_context(|| format!("could not read {}", capability_path.display()))?;
    let capabilities: CapabilityCatalogFile = toml::from_str(&capability_text)
        .with_context(|| format!("could not parse {}", capability_path.display()))?;

    let delivery_path = checked_repo_path(root, &product_roadmap.delivery_plan, "delivery plan")?;
    let delivery_text = fs::read_to_string(&delivery_path)
        .with_context(|| format!("could not read {}", delivery_path.display()))?;
    let delivery: DeliveryPlanFile = toml::from_str(&delivery_text)
        .with_context(|| format!("could not parse {}", delivery_path.display()))?;

    validate_product_roadmap(
        root,
        &certification,
        &product_roadmap,
        &capabilities,
        &delivery,
    )?;
    validate_module_profiles(root, &certification)?;
    validate_authoring_assets(root, &certification)?;
    validate_quality_matrix(root, &certification)?;
    let studio = load_and_validate_studio_parity(root)?;
    validate_product_certification(root, &product_roadmap)?;
    validate_workspace_version(root, &certification.framework_version)?;
    Ok((
        certification,
        roadmap,
        product_roadmap,
        capabilities,
        delivery,
        studio,
    ))
}

fn read_json_artifact(root: &Path, relative: &str) -> Result<serde_json::Value> {
    let path = checked_repo_path(root, relative, "UI authoring asset")?;
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    serde_json::from_str(&text).with_context(|| format!("could not parse {}", path.display()))
}

fn validate_authoring_assets(root: &Path, certification: &CertificationFile) -> Result<()> {
    let package = read_json_artifact(root, "ui/editors/vscode/package.json")?;
    if package.get("name").and_then(serde_json::Value::as_str) != Some("volang-ui-authoring")
        || package.get("publisher").and_then(serde_json::Value::as_str) != Some("vo-lang")
        || package.get("version").and_then(serde_json::Value::as_str)
            != Some(certification.framework_version.as_str())
    {
        bail!("Volang UI editor package identity or version is invalid");
    }
    for forbidden in [
        "main",
        "browser",
        "scripts",
        "dependencies",
        "devDependencies",
    ] {
        if package.get(forbidden).is_some() {
            bail!("Volang UI editor package must remain zero-runtime; found {forbidden}");
        }
    }
    let contributes = package
        .get("contributes")
        .context("Volang UI editor package has no contributions")?;
    let languages = contributes
        .get("languages")
        .and_then(serde_json::Value::as_array)
        .context("Volang UI editor package has no language contribution")?;
    if !languages.iter().any(|language| {
        language.get("id").and_then(serde_json::Value::as_str) == Some("volang")
            && language
                .get("extensions")
                .and_then(serde_json::Value::as_array)
                .is_some_and(|extensions| {
                    extensions.iter().any(|value| value.as_str() == Some(".vo"))
                })
            && language
                .get("configuration")
                .and_then(serde_json::Value::as_str)
                == Some("./language-configuration.json")
    }) {
        bail!("Volang UI editor package does not own the canonical .vo language contribution");
    }
    let grammars = contributes
        .get("grammars")
        .and_then(serde_json::Value::as_array)
        .context("Volang UI editor package has no grammar contribution")?;
    if !grammars.iter().any(|grammar| {
        grammar.get("language").and_then(serde_json::Value::as_str) == Some("volang")
            && grammar.get("scopeName").and_then(serde_json::Value::as_str) == Some("source.volang")
            && grammar.get("path").and_then(serde_json::Value::as_str)
                == Some("./syntaxes/volang.tmLanguage.json")
    }) {
        bail!("Volang UI editor package grammar contribution is invalid");
    }
    let snippets = contributes
        .get("snippets")
        .and_then(serde_json::Value::as_array)
        .context("Volang UI editor package has no snippet contribution")?;
    if !snippets.iter().any(|snippet| {
        snippet.get("language").and_then(serde_json::Value::as_str) == Some("volang")
            && snippet.get("path").and_then(serde_json::Value::as_str) == Some("./snippets/ui.json")
    }) {
        bail!("Volang UI editor package snippet contribution is invalid");
    }

    let configuration = read_json_artifact(root, "ui/editors/vscode/language-configuration.json")?;
    if configuration.get("comments").is_none()
        || configuration
            .get("brackets")
            .and_then(serde_json::Value::as_array)
            .is_none()
        || configuration.get("indentationRules").is_none()
    {
        bail!("Volang UI editor language configuration is incomplete");
    }
    let grammar = read_json_artifact(root, "ui/editors/vscode/syntaxes/volang.tmLanguage.json")?;
    if grammar.get("scopeName").and_then(serde_json::Value::as_str) != Some("source.volang")
        || grammar
            .get("patterns")
            .and_then(serde_json::Value::as_array)
            .is_none_or(|patterns| patterns.len() < 6)
    {
        bail!("Volang UI editor grammar is incomplete");
    }
    let snippet_catalog = read_json_artifact(root, "ui/editors/vscode/snippets/ui.json")?;
    if snippet_catalog
        .as_object()
        .is_none_or(|snippets| snippets.len() < 3)
    {
        bail!("Volang UI editor snippet catalog is incomplete");
    }
    let readme_path = checked_repo_path(root, "ui/editors/vscode/README.md", "UI authoring asset")?;
    let readme = fs::read_to_string(&readme_path)
        .with_context(|| format!("could not read {}", readme_path.display()))?;
    if !readme.contains("zero-runtime")
        || !readme.contains("no executable extension host code")
        || !readme.contains("npm dependency")
    {
        bail!("Volang UI editor package must document its zero-runtime boundary");
    }
    Ok(())
}

fn validate_module_profiles(root: &Path, certification: &CertificationFile) -> Result<()> {
    let path = root.join("ui/module-profiles.toml");
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let catalog: ModuleProfilesFile =
        toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))?;
    if catalog.schema_version != 1 || catalog.module != "github.com/vo-lang/ui" {
        bail!("UI module profile schema or module identity is invalid");
    }
    let required_capabilities = MODULE_PROFILE_CAPABILITIES.map(str::to_string);
    let capability_ids = validate_required_set(
        "module profile capability",
        &required_capabilities,
        catalog
            .capability
            .iter()
            .map(|capability| capability.id.as_str()),
    )?;
    let required_profiles = MODULE_PROFILES.map(str::to_string);
    let profile_ids = validate_required_set(
        "module profile",
        &required_profiles,
        catalog.profile.iter().map(|profile| profile.id.as_str()),
    )?;
    if catalog.default_profile != "full" || !profile_ids.contains(catalog.default_profile.as_str())
    {
        bail!("UI module default profile must be full");
    }
    let allowed_targets = UI_TARGETS.into_iter().collect::<BTreeSet<_>>();
    let mut package_owners = BTreeMap::new();
    let by_capability = catalog
        .capability
        .iter()
        .map(|capability| (capability.id.as_str(), capability))
        .collect::<BTreeMap<_, _>>();
    for capability in &catalog.capability {
        validate_token("module capability id", &capability.id)?;
        validate_optional_unique_texts("module capability requirement", &capability.requires)?;
        validate_unique_texts("module capability package", &capability.packages)?;
        validate_unique_texts("module capability target", &capability.targets)?;
        for requirement in &capability.requires {
            if !capability_ids.contains(requirement.as_str()) || requirement == &capability.id {
                bail!(
                    "UI module capability {} has invalid requirement {requirement}",
                    capability.id
                );
            }
        }
        for package in &capability.packages {
            if package != "github.com/vo-lang/ui" && !package.starts_with("github.com/vo-lang/ui/")
            {
                bail!(
                    "UI module capability {} owns foreign package {package}",
                    capability.id
                );
            }
            if let Some(owner) = package_owners.insert(package.as_str(), capability.id.as_str()) {
                bail!(
                    "UI module package {package} is owned by both {owner} and {}",
                    capability.id
                );
            }
            if !root
                .join("ui")
                .join(
                    package
                        .trim_start_matches("github.com/vo-lang/ui")
                        .trim_start_matches('/'),
                )
                .exists()
            {
                bail!(
                    "UI module capability {} references missing package {package}",
                    capability.id
                );
            }
        }
        for target in &capability.targets {
            if !allowed_targets.contains(target.as_str()) {
                bail!(
                    "UI module capability {} has unknown target {target}",
                    capability.id
                );
            }
        }
    }
    fn visit<'a>(
        id: &'a str,
        catalog: &BTreeMap<&'a str, &'a ModuleProfileCapability>,
        visiting: &mut BTreeSet<&'a str>,
        visited: &mut BTreeSet<&'a str>,
    ) -> Result<()> {
        if visited.contains(id) {
            return Ok(());
        }
        if !visiting.insert(id) {
            bail!("UI module capability dependency cycle contains {id}");
        }
        for requirement in &catalog[id].requires {
            visit(requirement, catalog, visiting, visited)?;
        }
        visiting.remove(id);
        visited.insert(id);
        Ok(())
    }
    let mut visiting = BTreeSet::new();
    let mut visited = BTreeSet::new();
    for id in by_capability.keys().copied() {
        visit(id, &by_capability, &mut visiting, &mut visited)?;
    }

    let mut represented = BTreeSet::new();
    for profile in &catalog.profile {
        validate_token("module profile id", &profile.id)?;
        validate_unique_texts("module profile capability selection", &profile.capabilities)?;
        let selected = profile
            .capabilities
            .iter()
            .map(String::as_str)
            .collect::<BTreeSet<_>>();
        for capability in &selected {
            if !capability_ids.contains(capability) {
                bail!(
                    "UI module profile {} selects unknown capability {capability}",
                    profile.id
                );
            }
            represented.insert(*capability);
            for requirement in &by_capability[capability].requires {
                if !selected.contains(requirement.as_str()) {
                    bail!(
                        "UI module profile {} selects {capability} without required {requirement}",
                        profile.id
                    );
                }
            }
        }
    }
    if represented != capability_ids {
        bail!("Every UI module capability must appear in a profile");
    }
    validate_text("UI certification product", &certification.product)?;
    Ok(())
}

fn validate_quality_matrix(root: &Path, certification: &CertificationFile) -> Result<()> {
    let path = root.join("ui/quality-matrix.toml");
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let matrix: QualityMatrixFile =
        toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))?;
    if matrix.schema_version != 1 || matrix.product != certification.product {
        bail!("UI quality matrix schema or product identity is invalid");
    }
    let required_suites = QUALITY_SUITES.map(str::to_string);
    validate_required_set(
        "quality suite",
        &required_suites,
        matrix.suite.iter().map(|suite| suite.id.as_str()),
    )?;
    let allowed_targets = QUALITY_TARGETS.into_iter().collect::<BTreeSet<_>>();
    for suite in &matrix.suite {
        validate_token("quality suite id", &suite.id)?;
        validate_unique_texts("quality suite target", &suite.targets)?;
        validate_unique_texts("quality suite command", &suite.commands)?;
        validate_unique_texts("quality suite evidence", &suite.evidence)?;
        for target in &suite.targets {
            if !allowed_targets.contains(target.as_str()) {
                bail!("UI quality suite {} has unknown target {target}", suite.id);
            }
        }
        for command in &suite.commands {
            if command.contains(['\r', '\n']) {
                bail!("UI quality suite {} has a multiline command", suite.id);
            }
        }
        for evidence in &suite.evidence {
            validate_evidence_reference(root, evidence, &format!("UI quality suite {}", suite.id))?;
        }
    }
    Ok(())
}

fn load_and_validate_studio_parity(root: &Path) -> Result<StudioParityFile> {
    let product_path = root.join("apps/studio/product.toml");
    let product_text = fs::read_to_string(&product_path)
        .with_context(|| format!("could not read {}", product_path.display()))?;
    let product: toml::Value = toml::from_str(&product_text)
        .with_context(|| format!("could not parse {}", product_path.display()))?;
    let matrix_relative = product
        .get("parity")
        .and_then(|value| value.get("matrix"))
        .and_then(toml::Value::as_str)
        .context("Studio product parity matrix is missing")?;
    if !Path::new(matrix_relative)
        .components()
        .all(|component| matches!(component, Component::Normal(_)))
    {
        bail!("Studio product parity matrix must be relative to apps/studio");
    }
    let matrix_path = root.join("apps/studio").join(matrix_relative);
    let matrix_text = fs::read_to_string(&matrix_path)
        .with_context(|| format!("could not read {}", matrix_path.display()))?;
    let matrix: StudioParityFile = toml::from_str(&matrix_text)
        .with_context(|| format!("could not parse {}", matrix_path.display()))?;
    if matrix.schema != "volang.studio.product-parity.v1" {
        bail!("Studio product parity schema is invalid");
    }
    validate_text("Studio product parity baseline", &matrix.baseline)?;
    let expected = STUDIO_CAPABILITIES.map(str::to_string);
    validate_required_set(
        "Studio product capability",
        &expected,
        matrix
            .capability
            .iter()
            .map(|capability| capability.id.as_str()),
    )?;
    let targets = STUDIO_TARGETS.into_iter().collect::<BTreeSet<_>>();
    for capability in &matrix.capability {
        validate_token("Studio capability id", &capability.id)?;
        if capability.status != "implemented" {
            bail!(
                "Studio capability {} remains {} in a complete product matrix",
                capability.id,
                capability.status
            );
        }
        validate_text("Studio capability acceptance", &capability.acceptance)?;
        validate_unique_texts("Studio capability target", &capability.targets)?;
        validate_unique_texts("Studio capability evidence", &capability.evidence)?;
        for target in &capability.targets {
            if !targets.contains(target.as_str()) {
                bail!(
                    "Studio capability {} has unknown target {target}",
                    capability.id
                );
            }
        }
        for evidence in &capability.evidence {
            validate_evidence_reference(
                root,
                evidence,
                &format!("Studio capability {}", capability.id),
            )?;
        }
    }
    Ok(matrix)
}

fn collect_generated_evidence(value: &toml::Value, references: &mut BTreeSet<String>) {
    match value {
        toml::Value::String(value) if value.starts_with(GENERATED_EVIDENCE_PREFIX) => {
            references.insert(value.clone());
        }
        toml::Value::Array(values) => {
            for value in values {
                collect_generated_evidence(value, references);
            }
        }
        toml::Value::Table(values) => {
            for value in values.values() {
                collect_generated_evidence(value, references);
            }
        }
        _ => {}
    }
}

fn validate_generated_evidence_artifacts(root: &Path) -> Result<usize> {
    let mut references = BTreeSet::new();
    for relative in [
        "ui/certification.toml",
        "ui/roadmap.toml",
        "ui/product-roadmap.toml",
        "ui/capabilities.toml",
        "ui/delivery.toml",
        "ui/kit/catalog.toml",
        "ui/quality-matrix.toml",
        "apps/studio/product-parity.toml",
    ] {
        let path = root.join(relative);
        let text = fs::read_to_string(&path)
            .with_context(|| format!("could not read {}", path.display()))?;
        let value: toml::Value =
            toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))?;
        collect_generated_evidence(&value, &mut references);
    }
    if references.is_empty() {
        bail!("UI certification declares no generated evidence reports");
    }
    for reference in &references {
        validate_evidence_reference(root, reference, "generated evidence gate")?;
        let relative = reference
            .strip_prefix(GENERATED_EVIDENCE_PREFIX)
            .expect("generated reference was collected by prefix");
        let path = checked_repo_path(root, relative, "generated evidence report")?;
        let metadata = fs::metadata(&path)
            .with_context(|| format!("generated evidence is missing: {}", path.display()))?;
        if !metadata.is_file() || metadata.len() == 0 || metadata.len() > 64 * 1024 * 1024 {
            bail!(
                "generated evidence has invalid size or type: {}",
                path.display()
            );
        }
        let bytes = fs::read(&path)
            .with_context(|| format!("could not read generated evidence {}", path.display()))?;
        let report: serde_json::Value = serde_json::from_slice(&bytes)
            .with_context(|| format!("could not parse generated evidence {}", path.display()))?;
        let object = report
            .as_object()
            .with_context(|| format!("generated evidence must be an object: {}", path.display()))?;
        if let Some(passed) = object.get("passed") {
            if passed.as_bool() != Some(true) {
                bail!("generated evidence did not pass: {}", path.display());
            }
        } else if object
            .get("schema")
            .and_then(serde_json::Value::as_str)
            .is_none_or(str::is_empty)
        {
            bail!(
                "generated evidence needs passed=true or a schema: {}",
                path.display()
            );
        }
    }
    Ok(references.len())
}

fn validate_certification(root: &Path, certification: &CertificationFile) -> Result<()> {
    if certification.schema_version != 1 {
        bail!("ui certification schema_version must be 1");
    }
    validate_text("product", &certification.product)?;
    validate_text("framework_version", &certification.framework_version)?;
    if certification.certification_level != "foundation" {
        bail!("UI certification level must be foundation until the product roadmap is complete");
    }
    if certification.product_roadmap == certification.roadmap {
        bail!("UI foundation and product roadmaps must be distinct");
    }
    if certification.protocols.as_slice() != REQUIRED_PROTOCOLS {
        bail!("ui certification protocols must use the canonical ordered set");
    }
    let mut gate_ids = BTreeSet::new();
    for gate in &certification.gate {
        validate_token("gate id", &gate.id)?;
        validate_text("gate summary", &gate.summary)?;
        if !gate_ids.insert(gate.id.as_str()) {
            bail!("duplicate UI certification gate {}", gate.id);
        }
        if gate.commands.is_empty() || gate.evidence.is_empty() {
            bail!(
                "UI certification gate {} needs commands and evidence",
                gate.id
            );
        }
        for command in &gate.commands {
            validate_text("gate command", command)?;
            if command.contains(['\r', '\n']) {
                bail!("UI certification gate {} has a multiline command", gate.id);
            }
        }
        for evidence in &gate.evidence {
            validate_evidence_reference(
                root,
                evidence,
                &format!("UI certification gate {}", gate.id),
            )?;
        }
    }
    Ok(())
}

fn validate_roadmap(certification: &CertificationFile, roadmap: &RoadmapFile) -> Result<()> {
    if roadmap.scope != "foundation-and-end-to-end-baseline"
        || roadmap.completion_claim != "foundation-certified"
        || !roadmap.frozen
        || roadmap.product_status != "in-progress"
        || roadmap.product_roadmap != certification.product_roadmap
    {
        bail!("UI baseline roadmap must identify its foundation scope and active product roadmap");
    }
    if roadmap.capability_catalog != "ui/capabilities.toml"
        || roadmap.delivery_plan != "ui/delivery.toml"
    {
        bail!("UI baseline roadmap must point at the active capability and delivery contracts");
    }
    let required_milestones: BTreeSet<_> = roadmap
        .completion
        .required_milestones
        .iter()
        .map(String::as_str)
        .collect();
    let actual_milestones: BTreeSet<_> = roadmap
        .milestone
        .iter()
        .map(|milestone| milestone.id.as_str())
        .collect();
    if required_milestones != actual_milestones {
        bail!("UI roadmap milestone set does not match its completion contract");
    }
    for milestone in &roadmap.milestone {
        if milestone.status != "complete" {
            bail!(
                "UI roadmap milestone {} is still {}",
                milestone.id,
                milestone.status
            );
        }
    }
    for item in &roadmap.work_item {
        if item.status != "complete" || !item.remaining.is_empty() {
            bail!("UI roadmap work item {} is incomplete", item.id);
        }
    }
    let required_gates: BTreeSet<_> = roadmap
        .completion
        .required_gates
        .iter()
        .map(String::as_str)
        .collect();
    let certified_gates: BTreeSet<_> = certification
        .gate
        .iter()
        .map(|gate| gate.id.as_str())
        .collect();
    if required_gates != certified_gates {
        bail!("UI certification gates do not match roadmap completion.required_gates");
    }
    Ok(())
}

fn validate_product_roadmap(
    root: &Path,
    certification: &CertificationFile,
    roadmap: &ProductRoadmapFile,
    capabilities: &CapabilityCatalogFile,
    delivery: &DeliveryPlanFile,
) -> Result<()> {
    if roadmap.schema_version != 2 {
        bail!("UI product roadmap schema_version must be 2");
    }
    if roadmap.product != certification.product {
        bail!("UI product roadmap product differs from certification");
    }
    validate_text("product target version", &roadmap.target_version)?;
    validate_text("product principle", &roadmap.product_principle)?;
    if roadmap.baseline_roadmap != certification.roadmap {
        bail!("UI product roadmap points at a different baseline roadmap");
    }
    if roadmap.capability_catalog != "ui/capabilities.toml"
        || roadmap.delivery_plan != "ui/delivery.toml"
        || roadmap.uikit_catalog != "ui/kit/catalog.toml"
    {
        bail!("UI product roadmap must point at the canonical capability, delivery, and UIKit contracts");
    }
    if roadmap.capability_catalog == roadmap.delivery_plan
        || roadmap.capability_catalog == roadmap.baseline_roadmap
        || roadmap.delivery_plan == roadmap.baseline_roadmap
    {
        bail!("UI product governance files must have distinct repository paths");
    }
    validate_progress_status("product roadmap", &roadmap.status)?;
    validate_unique_texts("benchmark family", &roadmap.benchmark_families)?;

    let required_domains = validate_required_set(
        "product domain",
        &roadmap.completion.required_domains,
        roadmap.domain.iter().map(|domain| domain.id.as_str()),
    )?;
    validate_required_set(
        "product showcase",
        &roadmap.completion.required_showcases,
        roadmap.showcase.iter().map(|showcase| showcase.id.as_str()),
    )?;
    validate_required_set(
        "product gate",
        &roadmap.completion.required_gates,
        roadmap.gate.iter().map(|gate| gate.id.as_str()),
    )?;

    for domain in &roadmap.domain {
        validate_token("product domain id", &domain.id)?;
        validate_text("product domain name", &domain.name)?;
        validate_unique_texts("product domain benchmark", &domain.benchmarks)?;
        validate_text("product domain outcome", &domain.outcome)?;
    }
    for showcase in &roadmap.showcase {
        validate_token("product showcase id", &showcase.id)?;
        validate_text("product showcase name", &showcase.name)?;
        validate_progress_status("product showcase", &showcase.status)?;
        validate_increment_id("product showcase starts_in", &showcase.starts_in)?;
        validate_unique_texts("product showcase acceptance", &showcase.acceptance)?;
        validate_completion_evidence(
            root,
            "product showcase",
            &showcase.id,
            &showcase.status,
            &showcase.commands,
            &showcase.evidence,
        )?;
    }
    for gate in &roadmap.gate {
        validate_token("product gate id", &gate.id)?;
        validate_progress_status("product gate", &gate.status)?;
        validate_unique_texts("product gate acceptance", &gate.acceptance)?;
        validate_completion_evidence(
            root,
            "product gate",
            &gate.id,
            &gate.status,
            &gate.commands,
            &gate.evidence,
        )?;
    }

    validate_capability_catalog(root, roadmap, capabilities, &required_domains, delivery)?;
    validate_delivery_plan(roadmap, capabilities, delivery)?;
    let uikit_market_complete = validate_uikit_catalog(root, roadmap, delivery)?;

    if roadmap.completion.required_capability_maturity != "stable" {
        bail!("UI 1.0 requires stable capability maturity");
    }
    if roadmap.completion.required_delivery_increment != "E8" {
        bail!("UI 1.0 requires delivery increment E8");
    }
    let capabilities_complete = capabilities
        .capability
        .iter()
        .filter(|capability| capability.required_for_1_0)
        .all(|capability| capability.status == roadmap.completion.required_capability_maturity);
    let delivery_complete = delivery
        .increment
        .iter()
        .find(|increment| increment.id == roadmap.completion.required_delivery_increment)
        .is_some_and(|increment| increment.status == "complete");
    let all_complete = capabilities_complete
        && delivery_complete
        && uikit_market_complete
        && roadmap
            .showcase
            .iter()
            .all(|showcase| showcase.status == "complete")
        && roadmap.gate.iter().all(|gate| gate.status == "complete");
    if (roadmap.status == "complete") != all_complete {
        bail!(
            "UI product status disagrees with capability, delivery, showcase, or gate completion"
        );
    }
    if roadmap.status == "complete" {
        for document in PRODUCT_DOCUMENTS {
            if !checked_repo_path(root, document, "product document")?.is_file() {
                bail!("UI product certification is missing document {document}");
            }
        }
    }
    Ok(())
}

fn validate_completion_evidence(
    root: &Path,
    kind: &str,
    id: &str,
    status: &str,
    commands: &[String],
    evidence: &[String],
) -> Result<()> {
    validate_optional_unique_texts(&format!("{kind} command"), commands)?;
    validate_optional_unique_texts(&format!("{kind} evidence"), evidence)?;
    if status == "complete" && (commands.is_empty() || evidence.is_empty()) {
        bail!("complete UI {kind} {id} needs commands and evidence");
    }
    for command in commands {
        if command.contains(['\r', '\n']) {
            bail!("UI {kind} {id} has a multiline command");
        }
    }
    for item in evidence {
        validate_evidence_reference(root, item, &format!("UI {kind} {id}"))?;
    }
    Ok(())
}

fn validate_product_certification(root: &Path, roadmap: &ProductRoadmapFile) -> Result<()> {
    let path = root.join("ui/product-certification.toml");
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let contract: ProductCertificationFile =
        toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))?;
    if contract.schema_version != 1
        || contract.product != roadmap.product
        || contract.target_version != roadmap.target_version
        || contract.status != "enforced"
        || contract.candidate_identity != "tagged-commit-with-successful-protected-main-ci"
        || contract.evidence_schema != "volang.ui.product-evidence.v1"
        || contract.quality_matrix != "ui/quality-matrix.toml"
        || contract.release_workflow != ".github/workflows/release.yml"
    {
        bail!("UI product certification contract identity or policy is invalid");
    }
    validate_required_set(
        "product certification gate",
        &contract.required_gates,
        roadmap.gate.iter().map(|gate| gate.id.as_str()),
    )?;
    validate_required_set(
        "product certification showcase",
        &contract.required_showcases,
        roadmap.showcase.iter().map(|showcase| showcase.id.as_str()),
    )?;
    validate_unique_texts(
        "product certification artifact",
        &contract.required_artifacts,
    )?;
    if contract.required_artifacts.len() < 6 {
        bail!("UI product certification must bind archives, digests, provenance, summary, attestation, and immutable publication");
    }
    for required in [&contract.quality_matrix, &contract.release_workflow] {
        if !checked_repo_path(root, required, "product certification policy")?.is_file() {
            bail!("UI product certification references missing policy {required}");
        }
    }
    Ok(())
}

fn validate_uikit_catalog(
    root: &Path,
    roadmap: &ProductRoadmapFile,
    delivery: &DeliveryPlanFile,
) -> Result<bool> {
    let path = checked_repo_path(root, &roadmap.uikit_catalog, "UIKit catalog")?;
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let catalog: UIKitCatalogFile =
        toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))?;
    if catalog.schema_version != 2 || catalog.product != roadmap.product || catalog.delivery != "E3"
    {
        bail!("UI UIKit catalog schema, product, or delivery differs from the product roadmap");
    }
    validate_progress_status("UIKit catalog", &catalog.status)?;
    validate_progress_status("UIKit market baseline", &catalog.market_baseline.status)?;
    validate_unique_texts(
        "UIKit market benchmark",
        &catalog.market_baseline.benchmarks,
    )?;
    validate_unique_texts(
        "UIKit market component",
        &catalog.market_baseline.components,
    )?;
    validate_unique_texts(
        "UIKit quality dimension",
        &catalog.market_baseline.quality_dimensions,
    )?;
    if catalog.market_baseline.benchmarks.len() < 4
        || catalog.market_baseline.components.len() < 40
        || catalog.market_baseline.quality_dimensions.len() < 8
    {
        bail!("UIKit market baseline is too narrow to represent a mainstream product surface");
    }
    let required_families = [
        "content",
        "form",
        "feedback",
        "overlay",
        "navigation",
        "data",
    ];
    validate_exact_order("UIKit family", &catalog.families, &required_families)?;
    validate_exact_order("UIKit target", &catalog.targets, &UIKIT_TARGETS)?;
    validate_exact_order(
        "UIKit theme",
        &catalog.themes,
        &["light", "dark", "high-contrast"],
    )?;
    validate_exact_order(
        "UIKit density",
        &catalog.densities,
        &["compact", "comfortable", "spacious"],
    )?;
    validate_exact_order("UIKit direction", &catalog.directions, &["ltr", "rtl"])?;

    let mut ids = BTreeSet::new();
    let mut represented_families = BTreeSet::new();
    for component in &catalog.component {
        validate_token("UIKit component id", &component.id)?;
        if !ids.insert(component.id.as_str()) {
            bail!("duplicate UIKit component {}", component.id);
        }
        if !required_families.contains(&component.family.as_str()) {
            bail!(
                "UIKit component {} has unknown family {}",
                component.id,
                component.family
            );
        }
        represented_families.insert(component.family.as_str());
        validate_enum(
            "UIKit component status",
            &component.status,
            &["planned", "implemented", "conformant"],
        )?;
        validate_text("UIKit component API", &component.api)?;
        validate_text("UIKit component behavior", &component.behavior)?;
        validate_unique_texts("UIKit component variant", &component.variants)?;
        validate_unique_texts("UIKit component state", &component.states)?;
        validate_unique_texts("UIKit component keyboard contract", &component.keyboard)?;
        validate_unique_texts("UIKit component semantic contract", &component.semantics)?;
        validate_optional_unique_texts("UIKit component evidence", &component.evidence)?;
        if component.status != "planned" && component.evidence.is_empty() {
            bail!(
                "UIKit component {} needs implementation evidence",
                component.id
            );
        }
        for evidence in &component.evidence {
            validate_evidence_reference(
                root,
                evidence,
                &format!("UIKit component {}", component.id),
            )?;
        }
    }
    let mut gap_ids = BTreeSet::new();
    for gap in &catalog.parity_gap {
        validate_token("UIKit parity gap id", &gap.id)?;
        if ids.contains(gap.id.as_str()) || !gap_ids.insert(gap.id.as_str()) {
            bail!(
                "duplicate or already implemented UIKit parity gap {}",
                gap.id
            );
        }
        validate_enum(
            "UIKit parity gap priority",
            &gap.priority,
            &["critical", "high", "normal"],
        )?;
        validate_unique_texts("UIKit parity gap benchmark", &gap.benchmarks)?;
        validate_unique_texts("UIKit parity gap acceptance", &gap.acceptance)?;
    }
    let baseline_ids = catalog
        .market_baseline
        .components
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let covered_ids = ids
        .iter()
        .copied()
        .chain(gap_ids.iter().copied())
        .collect::<BTreeSet<_>>();
    if baseline_ids != covered_ids {
        bail!(
            "UIKit implemented components and parity gaps must exactly cover the market baseline"
        );
    }
    if (catalog.market_baseline.status == "complete") != catalog.parity_gap.is_empty() {
        bail!("UIKit market parity may complete only after every governed gap is closed");
    }
    if represented_families.len() != required_families.len() {
        bail!("UIKit catalog must represent every Wave 1 family");
    }
    let e3_complete = delivery
        .increment
        .iter()
        .find(|increment| increment.id == "E3")
        .is_some_and(|increment| increment.status == "complete");
    let catalog_complete = catalog
        .component
        .iter()
        .all(|component| component.status == "conformant");
    if (catalog.status == "complete") != catalog_complete || e3_complete != catalog_complete {
        bail!("UIKit catalog, component rows, and E3 completion must advance together");
    }
    Ok(catalog.market_baseline.status == "complete")
}

fn validate_capability_catalog(
    root: &Path,
    roadmap: &ProductRoadmapFile,
    catalog: &CapabilityCatalogFile,
    required_domains: &BTreeSet<&str>,
    delivery: &DeliveryPlanFile,
) -> Result<()> {
    if catalog.schema_version != 1 {
        bail!("UI capability catalog schema_version must be 1");
    }
    if catalog.product != roadmap.product || catalog.target_version != roadmap.target_version {
        bail!("UI capability catalog product or target version differs from product roadmap");
    }
    validate_exact_order(
        "capability maturity",
        &catalog.maturity_order,
        &CAPABILITY_MATURITY,
    )?;
    validate_exact_order(
        "API stability",
        &catalog.api_stability_order,
        &API_STABILITY,
    )?;
    validate_exact_order("capability tier", &catalog.tiers, &CAPABILITY_TIERS)?;
    validate_exact_order("UI target", &catalog.targets, &UI_TARGETS)?;

    let delivery_rank = delivery
        .increment
        .iter()
        .enumerate()
        .map(|(rank, increment)| (increment.id.as_str(), rank))
        .collect::<BTreeMap<_, _>>();
    let target_set = catalog
        .targets
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let mut ids = BTreeSet::new();
    let mut domains_with_required_capabilities = BTreeSet::new();
    for capability in &catalog.capability {
        validate_token("capability id", &capability.id)?;
        if !ids.insert(capability.id.as_str()) {
            bail!("duplicate UI capability {}", capability.id);
        }
        if !required_domains.contains(capability.domain.as_str()) {
            bail!(
                "UI capability {} has unknown domain {}",
                capability.id,
                capability.domain
            );
        }
        if capability.required_for_1_0 {
            domains_with_required_capabilities.insert(capability.domain.as_str());
        }
        validate_text("capability name", &capability.name)?;
        validate_increment_id("capability delivery", &capability.delivery)?;
        if !delivery_rank.contains_key(capability.delivery.as_str()) {
            bail!(
                "UI capability {} has unknown delivery {}",
                capability.id,
                capability.delivery
            );
        }
        validate_enum(
            "capability maturity",
            &capability.status,
            &CAPABILITY_MATURITY,
        )?;
        validate_enum(
            "capability API stability",
            &capability.api_stability,
            &API_STABILITY,
        )?;
        validate_enum("capability tier", &capability.tier, &CAPABILITY_TIERS)?;
        validate_unique_texts("capability owner", &capability.owners)?;
        validate_unique_texts("capability required target", &capability.required_targets)?;
        validate_optional_unique_texts("capability optional target", &capability.optional_targets)?;
        validate_optional_unique_texts("capability dependency", &capability.depends_on)?;
        validate_unique_texts("capability acceptance", &capability.acceptance)?;
        validate_optional_unique_texts("capability evidence", &capability.evidence)?;
        let required_targets = capability
            .required_targets
            .iter()
            .map(String::as_str)
            .collect::<BTreeSet<_>>();
        for target in required_targets
            .iter()
            .copied()
            .chain(capability.optional_targets.iter().map(String::as_str))
        {
            if !target_set.contains(target) {
                bail!(
                    "UI capability {} has unknown target {target}",
                    capability.id
                );
            }
        }
        if capability
            .optional_targets
            .iter()
            .any(|target| required_targets.contains(target.as_str()))
        {
            bail!(
                "UI capability {} repeats a required target as optional",
                capability.id
            );
        }
        if capability.status != "specified" && capability.evidence.is_empty() {
            bail!(
                "UI capability {} needs evidence after specification",
                capability.id
            );
        }
        for evidence in &capability.evidence {
            validate_evidence_reference(
                root,
                evidence,
                &format!("UI capability {}", capability.id),
            )?;
        }
    }
    if domains_with_required_capabilities != *required_domains {
        bail!("Every required UI domain must own at least one required 1.0 capability");
    }

    let by_id = catalog
        .capability
        .iter()
        .map(|capability| (capability.id.as_str(), capability))
        .collect::<BTreeMap<_, _>>();
    for capability in &catalog.capability {
        let capability_rank = delivery_rank[capability.delivery.as_str()];
        for dependency in &capability.depends_on {
            let Some(dependency_capability) = by_id.get(dependency.as_str()) else {
                bail!(
                    "UI capability {} has unknown dependency {dependency}",
                    capability.id
                );
            };
            let dependency_rank = delivery_rank[dependency_capability.delivery.as_str()];
            if dependency_rank > capability_rank {
                bail!(
                    "UI capability {} depends on later delivery capability {dependency}",
                    capability.id
                );
            }
        }
        if capability.status == "stable" {
            if capability.required_for_1_0
                && !matches!(capability.api_stability.as_str(), "internal" | "stable")
            {
                bail!(
                    "stable UI capability {} still has {} public API stability",
                    capability.id,
                    capability.api_stability
                );
            }
            if delivery
                .increment
                .iter()
                .find(|increment| increment.id == capability.delivery)
                .is_none_or(|increment| increment.status != "complete")
            {
                bail!(
                    "stable UI capability {} belongs to incomplete delivery {}",
                    capability.id,
                    capability.delivery
                );
            }
            for dependency in &capability.depends_on {
                if by_id[dependency.as_str()].status != "stable" {
                    bail!(
                        "stable UI capability {} depends on non-stable {dependency}",
                        capability.id
                    );
                }
            }
        }
    }
    validate_capability_cycles(&by_id)?;
    Ok(())
}

fn validate_delivery_plan(
    roadmap: &ProductRoadmapFile,
    capabilities: &CapabilityCatalogFile,
    delivery: &DeliveryPlanFile,
) -> Result<()> {
    if delivery.schema_version != 1 {
        bail!("UI delivery plan schema_version must be 1");
    }
    if delivery.product != roadmap.product || delivery.target_version != roadmap.target_version {
        bail!("UI delivery plan product or target version differs from product roadmap");
    }
    validate_progress_status("delivery plan", &delivery.status)?;
    validate_increment_id("active delivery increment", &delivery.active_increment)?;

    let mut stream_ids = BTreeSet::new();
    for stream in &delivery.stream {
        validate_token("delivery stream id", &stream.id)?;
        validate_text("delivery stream name", &stream.name)?;
        if !stream_ids.insert(stream.id.as_str()) {
            bail!("duplicate UI delivery stream {}", stream.id);
        }
    }
    let increment_ids = delivery
        .increment
        .iter()
        .map(|increment| increment.id.as_str())
        .collect::<Vec<_>>();
    if increment_ids.as_slice() != DELIVERY_INCREMENTS {
        bail!("UI delivery increments must use the canonical E0 through E8 order");
    }
    let mut in_progress = Vec::new();
    for (rank, increment) in delivery.increment.iter().enumerate() {
        validate_text("delivery increment name", &increment.name)?;
        validate_progress_status("delivery increment", &increment.status)?;
        validate_optional_unique_texts("delivery dependency", &increment.depends_on)?;
        validate_unique_texts("delivery stream", &increment.streams)?;
        validate_unique_texts("delivery outcome", &increment.outcomes)?;
        validate_unique_texts("delivery exit", &increment.exit)?;
        if increment.status == "in-progress" {
            in_progress.push(increment.id.as_str());
        }
        for dependency in &increment.depends_on {
            let Some(dependency_rank) = DELIVERY_INCREMENTS
                .iter()
                .position(|candidate| candidate == dependency)
            else {
                bail!(
                    "UI increment {} has unknown dependency {dependency}",
                    increment.id
                );
            };
            if dependency_rank >= rank {
                bail!("UI increment {} dependency must precede it", increment.id);
            }
        }
        for stream in &increment.streams {
            if !stream_ids.contains(stream.as_str()) {
                bail!("UI increment {} has unknown stream {stream}", increment.id);
            }
        }
    }
    let active_rank = DELIVERY_INCREMENTS
        .iter()
        .position(|increment| *increment == delivery.active_increment)
        .context("UI active increment is missing")?;
    if delivery.status == "complete" {
        if !in_progress.is_empty() || delivery.active_increment != "E8" {
            bail!("complete UI delivery must have no in-progress increment and end at E8");
        }
        for increment in &delivery.increment {
            if increment.status != "complete" {
                bail!(
                    "complete UI delivery still has incomplete increment {}",
                    increment.id
                );
            }
        }
    } else {
        if in_progress.as_slice() != [delivery.active_increment.as_str()] {
            bail!("UI delivery plan must have exactly one active in-progress increment");
        }
        for (rank, increment) in delivery.increment.iter().enumerate() {
            if rank < active_rank && increment.status != "complete" {
                bail!(
                    "UI increment {} precedes the active increment but is incomplete",
                    increment.id
                );
            }
            if rank > active_rank && increment.status != "planned" {
                bail!(
                    "UI increment {} follows the active increment but is not planned",
                    increment.id
                );
            }
        }
    }

    let capability_ids = capabilities
        .capability
        .iter()
        .map(|capability| capability.id.as_str())
        .collect::<BTreeSet<_>>();
    let target_ids = capabilities
        .targets
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let mut probe_ids = BTreeSet::new();
    for probe in &delivery.contract_probe {
        if !probe_ids.insert(probe.id.as_str()) {
            bail!("duplicate UI contract probe {}", probe.id);
        }
    }
    let required_probe_ids = CONTRACT_PROBES.into_iter().collect::<BTreeSet<_>>();
    if probe_ids != required_probe_ids {
        bail!("UI delivery plan must contain the canonical E0 contract probe set");
    }
    for probe in &delivery.contract_probe {
        validate_token("contract probe id", &probe.id)?;
        validate_text("contract probe name", &probe.name)?;
        validate_progress_status("contract probe", &probe.status)?;
        if probe.increment != "E0" {
            bail!("UI contract probe {} must originate in E0", probe.id);
        }
        validate_unique_texts("contract probe capability", &probe.capabilities)?;
        validate_unique_texts("contract probe target", &probe.targets)?;
        validate_unique_texts("contract probe acceptance", &probe.acceptance)?;
        for capability in &probe.capabilities {
            if !capability_ids.contains(capability.as_str()) {
                bail!(
                    "UI contract probe {} has unknown capability {capability}",
                    probe.id
                );
            }
        }
        for target in &probe.targets {
            if !target_ids.contains(target.as_str()) {
                bail!("UI contract probe {} has unknown target {target}", probe.id);
            }
        }
    }

    let all_complete = delivery
        .increment
        .iter()
        .all(|increment| increment.status == "complete")
        && delivery
            .contract_probe
            .iter()
            .all(|probe| probe.status == "complete");
    if (delivery.status == "complete") != all_complete {
        bail!("UI delivery status disagrees with increments or contract probes");
    }
    Ok(())
}

fn validate_capability_cycles<'a>(
    capabilities: &BTreeMap<&'a str, &'a ProductCapability>,
) -> Result<()> {
    fn visit<'a>(
        id: &'a str,
        capabilities: &BTreeMap<&'a str, &'a ProductCapability>,
        visiting: &mut BTreeSet<&'a str>,
        visited: &mut BTreeSet<&'a str>,
    ) -> Result<()> {
        if visited.contains(id) {
            return Ok(());
        }
        if !visiting.insert(id) {
            bail!("UI capability dependency cycle contains {id}");
        }
        for dependency in &capabilities[id].depends_on {
            visit(dependency, capabilities, visiting, visited)?;
        }
        visiting.remove(id);
        visited.insert(id);
        Ok(())
    }

    let mut visiting = BTreeSet::new();
    let mut visited = BTreeSet::new();
    for id in capabilities.keys().copied() {
        visit(id, capabilities, &mut visiting, &mut visited)?;
    }
    Ok(())
}

fn validate_workspace_version(root: &Path, expected: &str) -> Result<()> {
    let workspace_text = fs::read_to_string(root.join("Cargo.toml"))?;
    let workspace: toml::Value = toml::from_str(&workspace_text)?;
    let actual = workspace
        .get("workspace")
        .and_then(|value| value.get("package"))
        .and_then(|value| value.get("version"))
        .and_then(toml::Value::as_str)
        .context("workspace.package.version is missing")?;
    if actual != expected {
        bail!("UI certification version {expected} differs from workspace version {actual}");
    }

    let module_text = fs::read_to_string(root.join("ui/vo.mod"))?;
    let module: toml::Value = toml::from_str(&module_text)?;
    let module_version = module
        .get("version")
        .and_then(toml::Value::as_str)
        .context("ui/vo.mod version is missing")?;
    if module_version != expected {
        bail!("UI module version {module_version} differs from certification {expected}");
    }
    Ok(())
}

fn checked_repo_path(root: &Path, relative: &str, field: &str) -> Result<std::path::PathBuf> {
    let path = Path::new(relative);
    if path.is_absolute()
        || path.components().any(|component| {
            matches!(
                component,
                Component::ParentDir
                    | Component::CurDir
                    | Component::RootDir
                    | Component::Prefix(_)
            )
        })
    {
        bail!("{field} must be a clean repository-relative path: {relative}");
    }
    Ok(root.join(path))
}

fn validate_evidence_reference(root: &Path, reference: &str, field: &str) -> Result<()> {
    validate_text("evidence reference", reference)?;
    let (generated, reference) = reference
        .strip_prefix(GENERATED_EVIDENCE_PREFIX)
        .map_or((false, reference), |path| (true, path));
    let path = reference
        .split_once('#')
        .map_or(reference, |(path, _fragment)| path);
    let resolved = checked_repo_path(root, path, "evidence reference")?;
    if generated {
        if !path.starts_with(GENERATED_EVIDENCE_ROOT)
            || !path.ends_with(".json")
            || reference.contains('#')
        {
            bail!(
                "{field} has invalid generated evidence {reference}; expected a JSON report under {GENERATED_EVIDENCE_ROOT}"
            );
        }
    } else if !resolved.exists() {
        bail!("{field} references missing evidence {path}");
    }
    Ok(())
}

fn validate_token(field: &str, value: &str) -> Result<()> {
    validate_text(field, value)?;
    if !value
        .bytes()
        .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'-')
    {
        bail!("{field} contains invalid characters: {value}");
    }
    Ok(())
}

fn validate_text(field: &str, value: &str) -> Result<()> {
    if value.is_empty() || value.trim() != value {
        bail!("{field} must be non-empty and unpadded");
    }
    Ok(())
}

fn validate_progress_status(field: &str, value: &str) -> Result<()> {
    if !matches!(value, "planned" | "in-progress" | "complete") {
        bail!("{field} has invalid status {value}");
    }
    Ok(())
}

fn validate_increment_id(field: &str, value: &str) -> Result<()> {
    validate_enum(field, value, &DELIVERY_INCREMENTS)
}

fn validate_enum<const N: usize>(field: &str, value: &str, allowed: &[&str; N]) -> Result<()> {
    validate_text(field, value)?;
    if !allowed.contains(&value) {
        bail!("{field} has invalid value {value}");
    }
    Ok(())
}

fn validate_exact_order<const N: usize>(
    field: &str,
    actual: &[String],
    expected: &[&str; N],
) -> Result<()> {
    if !actual
        .iter()
        .map(String::as_str)
        .eq(expected.iter().copied())
    {
        bail!("{field} list differs from the canonical ordered set");
    }
    Ok(())
}

fn validate_required_set<'a>(
    field: &str,
    required: &'a [String],
    actual: impl Iterator<Item = &'a str>,
) -> Result<BTreeSet<&'a str>> {
    validate_unique_texts(field, required)?;
    let required = required.iter().map(String::as_str).collect::<BTreeSet<_>>();
    let mut actual_set = BTreeSet::new();
    for value in actual {
        if !actual_set.insert(value) {
            bail!("duplicate {field} {value}");
        }
    }
    if required != actual_set {
        bail!("{field} set does not match its completion contract");
    }
    Ok(required)
}

fn validate_unique_texts(field: &str, values: &[String]) -> Result<()> {
    if values.is_empty() {
        bail!("{field} list must not be empty");
    }
    let mut unique = BTreeSet::new();
    for value in values {
        validate_text(field, value)?;
        if !unique.insert(value.as_str()) {
            bail!("duplicate {field} {value}");
        }
    }
    Ok(())
}

fn validate_optional_unique_texts(field: &str, values: &[String]) -> Result<()> {
    if values.is_empty() {
        return Ok(());
    }
    validate_unique_texts(field, values)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repository_ui_foundation_and_product_roadmaps_are_consistent() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        load_and_validate(&root).unwrap();
    }

    #[test]
    fn generated_evidence_is_declarative_and_path_constrained() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        assert!(validate_evidence_reference(
            &root,
            "generated:target/rewrite-validation/browser-report.json",
            "test gate",
        )
        .is_ok());
        assert!(validate_evidence_reference(
            &root,
            "generated:docs/browser-report.json",
            "test gate",
        )
        .is_err());
        assert!(validate_evidence_reference(
            &root,
            "generated:target/rewrite-validation/browser-report.txt",
            "test gate",
        )
        .is_err());
    }
}
