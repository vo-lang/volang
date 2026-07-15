use crate::config::ReleaseFile;
use crate::release_identity::workspace_version;
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Serialize)]
struct SdkPublishPlan {
    schema: u32,
    version: String,
    registry: String,
    preflight: Vec<String>,
    packages: Vec<SdkPublishStep>,
}

#[derive(Debug, Serialize)]
struct SdkPublishStep {
    order: usize,
    name: String,
    dependencies: Vec<String>,
    command: String,
}

#[derive(Debug, Deserialize)]
struct CargoMetadata {
    packages: Vec<CargoPackage>,
    workspace_members: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct CargoPackage {
    id: String,
    name: String,
    version: String,
    edition: String,
    manifest_path: PathBuf,
    publish: Option<Vec<String>>,
    rust_version: Option<String>,
    license: Option<String>,
    repository: Option<String>,
    homepage: Option<String>,
    description: Option<String>,
    readme: Option<PathBuf>,
    dependencies: Vec<CargoDependency>,
}

#[derive(Debug, Deserialize)]
struct CargoDependency {
    name: String,
    req: String,
    kind: Option<String>,
}

#[derive(Debug)]
struct WorkspacePackageDefaults {
    version: String,
    edition: String,
    rust_version: String,
    license: String,
    repository: String,
    homepage: String,
}

pub(crate) fn validate_sdk_publish_boundary(root: &Path, release: &ReleaseFile) -> Result<()> {
    build_sdk_publish_plan(root, release).map(|_| ())
}

pub(crate) fn cmd_sdk_plan(root: &Path, release: &ReleaseFile, args: Vec<String>) -> Result<()> {
    let mut json = false;
    let mut check = false;
    for arg in args {
        match arg.as_str() {
            "--json" => json = true,
            "--check" => check = true,
            other => bail!("unknown release sdk-plan argument: {other}"),
        }
    }
    if json && check {
        bail!("release sdk-plan accepts only one of --json and --check");
    }
    let plan = build_sdk_publish_plan(root, release)?;
    if check {
        println!(
            "SDK publish plan verified: {} crates at {} for {}",
            plan.packages.len(),
            plan.version,
            plan.registry
        );
        return Ok(());
    }
    if json {
        println!("{}", serde_json::to_string_pretty(&plan)?);
        return Ok(());
    }

    println!(
        "SDK {} publish plan for {} (verification only; this command never publishes):",
        plan.version, plan.registry
    );
    for command in &plan.preflight {
        println!("  {command}");
    }
    println!(
        "After preflight succeeds, publish each crate in order and wait for registry indexing:"
    );
    for package in &plan.packages {
        println!("  {:>2}. {}", package.order, package.command);
    }
    Ok(())
}

fn build_sdk_publish_plan(root: &Path, release: &ReleaseFile) -> Result<SdkPublishPlan> {
    let metadata = cargo_metadata(root)?;
    let defaults = workspace_package_defaults(root)?;
    let version = defaults.version.clone();
    let canonical_root = root
        .canonicalize()
        .context("could not canonicalize repository root")?;
    let members = metadata
        .workspace_members
        .iter()
        .cloned()
        .collect::<HashSet<_>>();
    let workspace_packages = metadata
        .packages
        .iter()
        .filter(|package| members.contains(&package.id))
        .collect::<Vec<_>>();
    if workspace_packages.len() != members.len() {
        bail!("cargo metadata omitted one or more workspace members");
    }

    let mut by_name = HashMap::new();
    for package in &workspace_packages {
        if by_name.insert(package.name.as_str(), *package).is_some() {
            bail!("workspace contains duplicate package name {}", package.name);
        }
        validate_workspace_package_metadata(package, &defaults, &canonical_root)?;
    }

    let sdk_names = release
        .sdk
        .packages
        .iter()
        .map(String::as_str)
        .collect::<HashSet<_>>();
    for package in &workspace_packages {
        if sdk_names.contains(package.name.as_str()) {
            let expected = vec![release.sdk.registry.clone()];
            if package.publish.as_ref() != Some(&expected) {
                bail!(
                    "public SDK package {} must set publish = [\"{}\"]",
                    package.name,
                    release.sdk.registry
                );
            }
            if package.readme.is_none() {
                bail!("public SDK package {} must declare a README", package.name);
            }
        } else if package.publish.as_ref() != Some(&Vec::new()) {
            bail!(
                "internal workspace package {} must explicitly set publish = false",
                package.name
            );
        }
    }
    validate_standalone_publish_boundary(root, release, &workspace_packages, &defaults)?;

    let mut index = HashMap::new();
    for (position, name) in release.sdk.packages.iter().enumerate() {
        if index.insert(name.as_str(), position).is_some() {
            bail!("duplicate SDK package {name}");
        }
        if !by_name.contains_key(name.as_str()) {
            bail!("SDK publish plan references non-workspace package {name}");
        }
    }

    let mut steps = Vec::with_capacity(release.sdk.packages.len());
    for (position, name) in release.sdk.packages.iter().enumerate() {
        let package = by_name[name.as_str()];
        let mut dependencies = Vec::new();
        for dependency in package
            .dependencies
            .iter()
            .filter(|dependency| dependency.kind.as_deref() != Some("dev"))
        {
            if let Some(dependency_index) = index.get(dependency.name.as_str()) {
                if *dependency_index >= position {
                    bail!(
                        "SDK publish order places {} before its dependency {}",
                        package.name,
                        dependency.name
                    );
                }
                if dependency.req != format!("={version}") {
                    bail!(
                        "SDK dependency {} -> {} must require ={version}, got {}",
                        package.name,
                        dependency.name,
                        dependency.req
                    );
                }
                dependencies.push(dependency.name.clone());
            } else if by_name.contains_key(dependency.name.as_str()) {
                bail!(
                    "public SDK package {} has a non-development dependency on internal workspace package {}",
                    package.name,
                    dependency.name
                );
            }
        }
        dependencies.sort();
        dependencies.dedup();
        steps.push(SdkPublishStep {
            order: position + 1,
            name: name.clone(),
            dependencies,
            command: format!(
                "cargo publish --locked --package {name} --registry {}",
                release.sdk.registry
            ),
        });
    }

    Ok(SdkPublishPlan {
        schema: 1,
        version,
        registry: release.sdk.registry.clone(),
        preflight: vec![
            "node scripts/ci/sdk_package_offline_consumer.mjs".to_string(),
            "cargo run -q -p vo-dev --offline --locked -- release sdk-plan --check".to_string(),
        ],
        packages: steps,
    })
}

fn cargo_metadata(root: &Path) -> Result<CargoMetadata> {
    let output = Command::new("cargo")
        .args([
            "metadata",
            "--offline",
            "--locked",
            "--format-version",
            "1",
            "--no-deps",
        ])
        .current_dir(root)
        .output()
        .context("could not run cargo metadata for SDK publish policy")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        bail!("cargo metadata for SDK publish policy failed: {stderr}");
    }
    if !output.stderr.is_empty() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        if !stderr.is_empty() {
            bail!("cargo metadata for SDK publish policy emitted warnings: {stderr}");
        }
    }
    serde_json::from_slice(&output.stdout).context("could not parse cargo metadata for SDK policy")
}

fn validate_workspace_package_metadata(
    package: &CargoPackage,
    defaults: &WorkspacePackageDefaults,
    canonical_root: &Path,
) -> Result<()> {
    if package.version != defaults.version {
        bail!(
            "workspace package {} has version {}, expected {}",
            package.name,
            package.version,
            defaults.version
        );
    }
    if package.edition != defaults.edition {
        bail!(
            "workspace package {} has edition {}, expected {}",
            package.name,
            package.edition,
            defaults.edition
        );
    }
    if package.rust_version.as_deref() != Some(defaults.rust_version.as_str())
        || package.license.as_deref() != Some(defaults.license.as_str())
        || package.repository.as_deref() != Some(defaults.repository.as_str())
        || package.homepage.as_deref() != Some(defaults.homepage.as_str())
        || package
            .description
            .as_deref()
            .is_none_or(|description| description.trim().is_empty())
    {
        bail!(
            "workspace package {} has incomplete canonical metadata",
            package.name
        );
    }
    let manifest = package
        .manifest_path
        .canonicalize()
        .with_context(|| format!("could not canonicalize {}", package.manifest_path.display()))?;
    if !manifest.starts_with(canonical_root) {
        bail!(
            "workspace manifest escapes repository root: {}",
            manifest.display()
        );
    }
    Ok(())
}

fn validate_standalone_publish_boundary(
    root: &Path,
    release: &ReleaseFile,
    workspace_packages: &[&CargoPackage],
    defaults: &WorkspacePackageDefaults,
) -> Result<()> {
    let canonical_root = root
        .canonicalize()
        .context("could not canonicalize repository root")?;
    let workspace_manifests = workspace_packages
        .iter()
        .map(|package| {
            package
                .manifest_path
                .canonicalize()
                .with_context(|| {
                    format!(
                        "could not canonicalize workspace manifest {}",
                        package.manifest_path.display()
                    )
                })?
                .strip_prefix(&canonical_root)
                .map(Path::to_path_buf)
                .context("workspace manifest escaped repository root")
        })
        .collect::<Result<HashSet<_>>>()?;

    let declared = release
        .sdk
        .internal_standalone
        .iter()
        .map(|path| PathBuf::from(path).join("Cargo.toml"))
        .collect::<HashSet<_>>();
    for manifest_relative in &declared {
        if workspace_manifests.contains(manifest_relative) {
            bail!(
                "standalone internal package is already a workspace member: {}",
                manifest_relative.display()
            );
        }
        let manifest_path = root.join(manifest_relative);
        let metadata = fs::symlink_metadata(&manifest_path)
            .with_context(|| format!("could not inspect {}", manifest_path.display()))?;
        if !metadata.file_type().is_file() {
            bail!(
                "standalone Cargo manifest must be a regular file: {}",
                manifest_path.display()
            );
        }
        let canonical_manifest = manifest_path
            .canonicalize()
            .with_context(|| format!("could not canonicalize {}", manifest_path.display()))?;
        if canonical_manifest != canonical_root.join(manifest_relative) {
            bail!(
                "standalone Cargo manifest must not traverse symlinks: {}",
                manifest_path.display()
            );
        }
        let text = fs::read_to_string(&manifest_path)
            .with_context(|| format!("could not read {}", manifest_path.display()))?;
        let manifest = text
            .parse::<toml::Table>()
            .with_context(|| format!("could not parse {}", manifest_path.display()))?;
        let package = manifest
            .get("package")
            .and_then(toml::Value::as_table)
            .ok_or_else(|| anyhow!("{} has no [package] table", manifest_path.display()))?;
        if package.get("publish").and_then(toml::Value::as_bool) != Some(false) {
            bail!(
                "standalone internal package must set publish = false: {}",
                manifest_path.display()
            );
        }
        for (field, expected) in [
            ("edition", defaults.edition.as_str()),
            ("rust-version", defaults.rust_version.as_str()),
            ("license", defaults.license.as_str()),
            ("repository", defaults.repository.as_str()),
            ("homepage", defaults.homepage.as_str()),
        ] {
            if package.get(field).and_then(toml::Value::as_str) != Some(expected) {
                bail!(
                    "standalone internal package {} must set {field} = {expected:?}",
                    manifest_path.display()
                );
            }
        }
        if package
            .get("description")
            .and_then(toml::Value::as_str)
            .is_none_or(|description| description.trim().is_empty())
        {
            bail!(
                "standalone internal package has no description: {}",
                manifest_path.display()
            );
        }
    }

    let manifests = repository_cargo_manifests(root, &canonical_root)?;
    let actual_standalone = manifests
        .difference(&workspace_manifests)
        .filter(|path| path.as_path() != Path::new("Cargo.toml"))
        .cloned()
        .collect::<HashSet<_>>();
    if actual_standalone != declared {
        let mut undeclared = actual_standalone
            .difference(&declared)
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>();
        let mut stale = declared
            .difference(&actual_standalone)
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>();
        undeclared.sort();
        stale.sort();
        bail!(
            "standalone Cargo publish policy drifted; undeclared: {}; stale declarations: {}",
            if undeclared.is_empty() {
                "none".to_string()
            } else {
                undeclared.join(", ")
            },
            if stale.is_empty() {
                "none".to_string()
            } else {
                stale.join(", ")
            }
        );
    }
    Ok(())
}

fn repository_cargo_manifests(root: &Path, canonical_root: &Path) -> Result<HashSet<PathBuf>> {
    let output = Command::new("git")
        .args([
            "ls-files",
            "-z",
            "--cached",
            "--others",
            "--exclude-standard",
            "--",
            "Cargo.toml",
            ":(glob)**/Cargo.toml",
        ])
        .current_dir(root)
        .output()
        .context("could not enumerate repository Cargo manifests")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        bail!("could not enumerate repository Cargo manifests: {stderr}");
    }
    let stdout = String::from_utf8(output.stdout).context("git ls-files output was not UTF-8")?;
    let mut manifests = HashSet::new();
    for path in stdout.split_terminator('\0') {
        let relative = PathBuf::from(path);
        if relative.as_path() != Path::new("Cargo.toml") && !path.ends_with("/Cargo.toml") {
            bail!("git returned a non-manifest path for Cargo policy: {path:?}");
        }
        let absolute = root.join(&relative);
        let metadata = fs::symlink_metadata(&absolute)
            .with_context(|| format!("could not inspect {}", absolute.display()))?;
        if !metadata.file_type().is_file() {
            bail!(
                "Cargo manifest must be a regular file: {}",
                absolute.display()
            );
        }
        let canonical = absolute
            .canonicalize()
            .with_context(|| format!("could not canonicalize {}", absolute.display()))?;
        if canonical != canonical_root.join(&relative) {
            bail!(
                "Cargo manifest must not traverse symlinks: {}",
                absolute.display()
            );
        }
        manifests.insert(relative);
    }
    Ok(manifests)
}

fn workspace_package_defaults(root: &Path) -> Result<WorkspacePackageDefaults> {
    let path = root.join("Cargo.toml");
    let manifest = std::fs::read_to_string(&path)
        .with_context(|| format!("could not read {}", path.display()))?
        .parse::<toml::Table>()
        .with_context(|| format!("could not parse {}", path.display()))?;
    let package = manifest
        .get("workspace")
        .and_then(toml::Value::as_table)
        .and_then(|workspace| workspace.get("package"))
        .and_then(toml::Value::as_table)
        .ok_or_else(|| anyhow!("Cargo.toml has no workspace.package table"))?;
    let field = |name: &str| -> Result<String> {
        package
            .get(name)
            .and_then(toml::Value::as_str)
            .filter(|value| !value.trim().is_empty() && value.trim() == *value)
            .map(str::to_string)
            .ok_or_else(|| anyhow!("Cargo.toml has no canonical workspace.package.{name}"))
    };
    Ok(WorkspacePackageDefaults {
        version: workspace_version(root)?,
        edition: field("edition")?,
        rust_version: field("rust-version")?,
        license: field("license")?,
        repository: field("repository")?,
        homepage: field("homepage")?,
    })
}
