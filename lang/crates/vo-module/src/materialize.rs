use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::attestation::{
    prepare_dev_materialization, publish_prepared_dev_materialization, AttestedRoleArtifact,
    DetachedArtifactManifest, DetachedProviderFactory, MaterializationOutput,
    PreparedDevMaterialization, StaticInitializerPolicy, DETACHED_ARTIFACT_MANIFEST_FORMAT,
};
use crate::digest::Digest;
use crate::profile::{ArtifactRole, CapabilitySet, ProfileCatalog, ResolvedSourceRecipe};
use crate::schema::lockfile::LockedModule;
use crate::schema::manifest::ReleaseManifest;
use crate::{Error, Result};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MaterializationDigestEntry {
    pub identity: String,
    pub digest: Digest,
}

#[derive(Clone, Debug)]
pub struct SourceRecipeBuildRequest<'a> {
    pub locked: &'a LockedModule,
    pub recipe: &'a ResolvedSourceRecipe,
    pub inputs: Digest,
    pub environment: Digest,
}

#[derive(Debug)]
pub struct BuiltRoleOutput {
    pub role: ArtifactRole,
    pub kind: String,
    pub name: String,
    pub bytes: Vec<u8>,
    pub detached_manifest: Vec<u8>,
}

pub trait SourceRecipeBuilder {
    fn build(&self, request: SourceRecipeBuildRequest<'_>) -> Result<Vec<BuiltRoleOutput>>;
}

#[derive(Clone, Debug)]
pub struct CargoSourceRecipeBuilder {
    pub source_root: PathBuf,
    pub cargo_manifest: PathBuf,
    pub package: String,
    pub profile_feature: String,
    pub target_dir: PathBuf,
    pub release: bool,
}

impl SourceRecipeBuilder for CargoSourceRecipeBuilder {
    fn build(&self, request: SourceRecipeBuildRequest<'_>) -> Result<Vec<BuiltRoleOutput>> {
        validate_cargo_builder(self, request.recipe)?;
        fs::create_dir_all(&self.target_dir)?;
        let mut command = Command::new("cargo");
        command
            .arg("build")
            .arg("--manifest-path")
            .arg(&self.cargo_manifest)
            .arg("--package")
            .arg(&self.package)
            .arg("--target")
            .arg(&request.recipe.target)
            .arg("--no-default-features")
            .arg("--features")
            .arg(&self.profile_feature)
            .arg("--locked")
            .arg("--offline")
            .env("CARGO_TARGET_DIR", &self.target_dir)
            .current_dir(&self.source_root);
        let render_feature_factories = linked_render_feature_factories(request.recipe)?;
        if !render_feature_factories.is_empty() {
            command.env(
                "VOPLAY_RENDER_FEATURE_FACTORIES",
                render_feature_factories.join(","),
            );
        }
        if self.release {
            command.arg("--release");
        }
        let output = command.output().map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "failed to execute Cargo source recipe for {}@{}: {error}",
                request.locked.path, request.locked.version
            ))
        })?;
        if !output.status.success() {
            let stderr =
                String::from_utf8_lossy(&output.stderr[..output.stderr.len().min(64 * 1024)]);
            return Err(Error::InvalidReleaseMetadata(format!(
                "Cargo source recipe failed for {}@{} with {}: {}",
                request.locked.path,
                request.locked.version,
                output.status,
                stderr.trim(),
            )));
        }
        self.collect_outputs(request)
    }
}

impl CargoSourceRecipeBuilder {
    fn collect_outputs(
        &self,
        request: SourceRecipeBuildRequest<'_>,
    ) -> Result<Vec<BuiltRoleOutput>> {
        let profile = if self.release { "release" } else { "debug" };
        let artifact_dir = self.target_dir.join(&request.recipe.target).join(profile);
        let crate_stem = self.package.replace('-', "_");
        let native_name = if request.recipe.target.starts_with("wasm32") {
            None
        } else {
            Some(platform_library_name(&crate_stem, &request.recipe.target)?)
        };
        let wasm_name = format!("{crate_stem}.wasm");
        let published_wasm_name = request
            .recipe
            .role_outputs
            .iter()
            .find(|output| output.kind == "extension-wasm")
            .map(|output| output.name.as_str())
            .unwrap_or(wasm_name.as_str());
        let mut cached = BTreeMap::<String, Vec<u8>>::new();
        let mut outputs = Vec::with_capacity(request.recipe.role_outputs.len());
        for declared in &request.recipe.role_outputs {
            let bytes = match declared.kind.as_str() {
                "extension-native" => read_bounded_artifact(
                    &artifact_dir.join(native_name.as_deref().ok_or_else(|| {
                        Error::InvalidReleaseMetadata(
                            "WASM source recipe requested a native output".to_string(),
                        )
                    })?),
                    &mut cached,
                )?,
                "extension-wasm" => {
                    read_bounded_artifact(&artifact_dir.join(&wasm_name), &mut cached)?
                }
                "extension-js-glue" => {
                    render_wasm_loader(request.locked, request.recipe, published_wasm_name)
                        .into_bytes()
                }
                kind => {
                    return Err(Error::InvalidReleaseMetadata(format!(
                        "Cargo source recipe does not support output kind {kind:?}"
                    )))
                }
            };
            let content_digest = Digest::from_sha256(&bytes);
            let artifact = AttestedRoleArtifact {
                role: declared.role.clone(),
                kind: declared.kind.clone(),
                name: declared.name.clone(),
                size: u64::try_from(bytes.len()).map_err(|_| {
                    Error::InvalidReleaseMetadata(
                        "materialized output size exceeds u64".to_string(),
                    )
                })?,
                content_digest: content_digest.clone(),
                detached_manifest_digest: Digest::from_sha256(b"pending-detached-manifest"),
            };
            let manifest = DetachedArtifactManifest {
                format: DETACHED_ARTIFACT_MANIFEST_FORMAT,
                module: request.locked.path.clone(),
                version: request.locked.version.clone(),
                role: declared.role.clone(),
                kind: declared.kind.clone(),
                name: declared.name.clone(),
                target: request.recipe.target.clone(),
                capabilities: request.recipe.capabilities.as_slice().to_vec(),
                schema: request.recipe.schema.clone(),
                abi: request.recipe.abi.clone(),
                content_digest,
                static_initializer_policy: StaticInitializerPolicy::ProvenAbsent,
                factories: vec![DetachedProviderFactory {
                    factory_id: stable_factory_id(
                        request.locked.path.as_str(),
                        declared.role.as_str(),
                    ),
                    role: declared.role.clone(),
                    export: "provider-factory-table-v1".to_string(),
                    abi: request.recipe.abi.clone(),
                    schema: request.recipe.schema.clone(),
                    capability_digest: request.recipe.capabilities.digest(),
                }],
            };
            outputs.push(BuiltRoleOutput {
                role: declared.role.clone(),
                kind: declared.kind.clone(),
                name: declared.name.clone(),
                bytes,
                detached_manifest: manifest.render(request.locked, &artifact)?,
            });
        }
        Ok(outputs)
    }
}

pub fn exact_source_profile_alias(
    release: &ReleaseManifest,
    recipe: &ResolvedSourceRecipe,
) -> Result<String> {
    let catalog = ProfileCatalog::from_declarations_with_default(
        release.profiles.clone(),
        release.default_profile.clone(),
        "source materialization profiles",
    )?;
    let mut matches = Vec::new();
    for name in release.profiles.keys() {
        let capabilities = catalog.resolve(
            Some(name),
            &CapabilitySet::default(),
            "source materialization profile",
        )?;
        if capabilities == recipe.capabilities {
            matches.push(name.clone());
        }
    }
    match matches.as_slice() {
        [name] => Ok(name.clone()),
        [] => Err(Error::InvalidReleaseMetadata(format!(
            "source recipe for {} has no exact profile alias",
            release.module
        ))),
        _ => Err(Error::InvalidReleaseMetadata(format!(
            "source recipe for {} ambiguously matches profiles {}",
            release.module,
            matches.join(", "),
        ))),
    }
}

fn validate_cargo_builder(
    builder: &CargoSourceRecipeBuilder,
    recipe: &ResolvedSourceRecipe,
) -> Result<()> {
    if !builder.source_root.is_absolute()
        || !builder.cargo_manifest.is_absolute()
        || !builder.target_dir.is_absolute()
        || builder.package.trim().is_empty()
        || builder.profile_feature.trim().is_empty()
        || !builder.profile_feature.starts_with("profile-")
        || builder.profile_feature != builder.profile_feature.trim()
        || recipe.role_outputs.is_empty()
    {
        return Err(Error::InvalidReleaseMetadata(
            "Cargo source recipe builder configuration is invalid".to_string(),
        ));
    }
    Ok(())
}

fn platform_library_name(crate_stem: &str, target: &str) -> Result<String> {
    if target.contains("apple") {
        Ok(format!("lib{crate_stem}.dylib"))
    } else if target.contains("windows") {
        Ok(format!("{crate_stem}.dll"))
    } else if target.starts_with("wasm32") {
        Err(Error::InvalidReleaseMetadata(
            "WASM targets do not produce native libraries".to_string(),
        ))
    } else {
        Ok(format!("lib{crate_stem}.so"))
    }
}

fn read_bounded_artifact(path: &Path, cache: &mut BTreeMap<String, Vec<u8>>) -> Result<Vec<u8>> {
    let identity = path.to_string_lossy().into_owned();
    if let Some(bytes) = cache.get(&identity) {
        return Ok(bytes.clone());
    }
    let metadata = fs::metadata(path)?;
    if !metadata.is_file()
        || metadata.len() == 0
        || metadata.len() > crate::MAX_MODULE_ARTIFACT_BYTES
    {
        return Err(Error::InvalidReleaseMetadata(format!(
            "materialized Cargo output {} must be a regular file of 1..={} bytes",
            path.display(),
            crate::MAX_MODULE_ARTIFACT_BYTES,
        )));
    }
    let bytes = fs::read(path)?;
    cache.insert(identity, bytes.clone());
    Ok(bytes)
}

fn render_wasm_loader(
    locked: &LockedModule,
    recipe: &ResolvedSourceRecipe,
    wasm_name: &str,
) -> String {
    let entrypoints = recipe
        .js_entrypoints
        .iter()
        .map(|entry| format!("{entry:?}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "// governed:vo-source-materializer module={} version={} recipe={}\n\
export const voProviderEntrypoints = Object.freeze([{}]);\n\
export async function instantiateVoProvider(source, imports = {{}}) {{\n\
  const bytes = source instanceof ArrayBuffer ? source : await (await fetch(source ?? new URL({:?}, import.meta.url))).arrayBuffer();\n\
  const result = await WebAssembly.instantiate(bytes, imports);\n\
  return result.instance ?? result;\n\
}}\n",
        locked.path,
        locked.version,
        recipe.recipe,
        entrypoints,
        wasm_name,
    )
}

fn stable_factory_id(module: &str, role: &str) -> u32 {
    let mut hash = 0x811c_9dc5u32;
    for byte in module
        .as_bytes()
        .iter()
        .chain([0].iter())
        .chain(role.as_bytes())
    {
        hash ^= u32::from(*byte);
        hash = hash.wrapping_mul(0x0100_0193);
    }
    hash.max(1)
}

pub fn materialization_digest(
    domain: &str,
    entries: &[MaterializationDigestEntry],
) -> Result<Digest> {
    crate::profile::validate_stable_name(domain, "materialization digest domain")?;
    if entries.is_empty() || entries.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::InvalidReleaseMetadata(format!(
            "materialization {domain} entries must contain 1..={} items",
            crate::MAX_MODULE_METADATA_ENTRIES,
        )));
    }
    let mut identities = BTreeSet::new();
    let mut previous = None;
    let mut bytes = b"vo-materialization-digest-v1\0".to_vec();
    bytes.extend_from_slice(domain.as_bytes());
    bytes.push(0);
    for entry in entries {
        if entry.identity.is_empty()
            || entry.identity.len() > 1024
            || entry.identity.trim() != entry.identity
            || entry.identity.chars().any(char::is_control)
            || previous.is_some_and(|value: &str| value >= entry.identity.as_str())
            || !identities.insert(entry.identity.as_str())
        {
            return Err(Error::InvalidReleaseMetadata(format!(
                "materialization {domain} entries must use sorted, unique canonical identities",
            )));
        }
        previous = Some(&entry.identity);
        bytes.extend_from_slice(entry.identity.as_bytes());
        bytes.push(0);
        bytes.extend_from_slice(entry.digest.as_str().as_bytes());
        bytes.push(0);
    }
    Ok(Digest::from_sha256(&bytes))
}

pub fn source_tree_input_entries(root: &Path) -> Result<Vec<MaterializationDigestEntry>> {
    if !root.is_absolute() {
        return Err(Error::InvalidReleaseMetadata(
            "source materialization root must be absolute".to_string(),
        ));
    }
    let mut pending = vec![root.to_path_buf()];
    let mut files = Vec::new();
    let mut total_bytes = 0u64;
    let max_source_bytes =
        u64::try_from(vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES).map_err(|_| {
            Error::InvalidReleaseMetadata("package source byte limit exceeds u64".to_string())
        })?;
    while let Some(directory) = pending.pop() {
        let mut entries = fs::read_dir(&directory)?.collect::<std::io::Result<Vec<_>>>()?;
        entries.sort_by_key(|entry| entry.file_name());
        for entry in entries {
            let path = entry.path();
            let relative = path.strip_prefix(root).map_err(|_| {
                Error::InvalidReleaseMetadata(
                    "source materialization path escaped its root".to_string(),
                )
            })?;
            if excluded_source_component(relative) {
                continue;
            }
            let metadata = fs::symlink_metadata(&path)?;
            if metadata.file_type().is_symlink() {
                return Err(Error::InvalidReleaseMetadata(format!(
                    "source materialization rejects symbolic link {}",
                    path.display()
                )));
            }
            if metadata.is_dir() {
                pending.push(path);
                continue;
            }
            if !metadata.is_file() {
                return Err(Error::InvalidReleaseMetadata(format!(
                    "source materialization rejects special entry {}",
                    path.display()
                )));
            }
            total_bytes = total_bytes.checked_add(metadata.len()).ok_or_else(|| {
                Error::InvalidReleaseMetadata(
                    "source materialization input byte count overflow".to_string(),
                )
            })?;
            if files.len() >= vo_common::vfs::MAX_PACKAGE_SOURCE_FILES
                || total_bytes > max_source_bytes
            {
                return Err(Error::InvalidReleaseMetadata(
                    "source materialization input tree exceeds package limits".to_string(),
                ));
            }
            let bytes = fs::read(&path)?;
            files.push(MaterializationDigestEntry {
                identity: relative.to_string_lossy().replace('\\', "/"),
                digest: Digest::from_sha256(&bytes),
            });
        }
    }
    files.sort_by(|left, right| left.identity.cmp(&right.identity));
    if files.is_empty() {
        return Err(Error::InvalidReleaseMetadata(
            "source materialization input tree is empty".to_string(),
        ));
    }
    Ok(files)
}

pub fn cargo_environment_entries(
    recipe: &ResolvedSourceRecipe,
    profile_feature: &str,
) -> Result<Vec<MaterializationDigestEntry>> {
    let cargo = command_version("cargo", &["--version"])?;
    let rustc = command_version("rustc", &["--version", "--verbose"])?;
    let mut entries = vec![
        environment_entry("cargo-version", cargo.as_bytes()),
        environment_entry("profile-feature", profile_feature.as_bytes()),
        environment_entry("recipe", recipe.recipe.as_str().as_bytes()),
        environment_entry("rustc-version", rustc.as_bytes()),
        environment_entry("target", recipe.target.as_bytes()),
        environment_entry("toolchain", recipe.toolchain.as_bytes()),
    ];
    let render_feature_factories = linked_render_feature_factories(recipe)?;
    if !render_feature_factories.is_empty() {
        entries.push(environment_entry(
            "voplay-render-feature-factories",
            render_feature_factories.join(",").as_bytes(),
        ));
    }
    entries.sort_by(|left, right| left.identity.cmp(&right.identity));
    Ok(entries)
}

const VOPLAY_RENDER_FEATURE_FACTORY_PREFIX: &str = "render-feature-factory:";

fn linked_render_feature_factories(recipe: &ResolvedSourceRecipe) -> Result<Vec<&str>> {
    let mut factories = recipe
        .cargo_features
        .iter()
        .filter_map(|entry| entry.strip_prefix(VOPLAY_RENDER_FEATURE_FACTORY_PREFIX))
        .collect::<Vec<_>>();
    for factory in &factories {
        if !valid_rust_item_path(factory) {
            return Err(Error::InvalidReleaseMetadata(format!(
                "source recipe contains invalid Voplay RenderFeature factory path {factory:?}"
            )));
        }
    }
    factories.sort_unstable();
    if factories.windows(2).any(|pair| pair[0] == pair[1]) {
        return Err(Error::InvalidReleaseMetadata(
            "source recipe contains duplicate Voplay RenderFeature factory paths".to_string(),
        ));
    }
    Ok(factories)
}

fn valid_rust_item_path(path: &str) -> bool {
    !path.is_empty()
        && path.split("::").all(|segment| {
            let mut characters = segment.chars();
            characters
                .next()
                .is_some_and(|first| first == '_' || first.is_ascii_alphabetic())
                && characters.all(|character| character == '_' || character.is_ascii_alphanumeric())
        })
}

fn excluded_source_component(relative: &Path) -> bool {
    relative.components().any(|component| {
        matches!(
            component.as_os_str().to_str(),
            Some(".git" | ".codex" | "target" | "node_modules" | "dist")
        )
    })
}

fn command_version(program: &str, arguments: &[&str]) -> Result<String> {
    let output = Command::new(program)
        .args(arguments)
        .output()
        .map_err(|error| {
            Error::InvalidReleaseMetadata(format!(
                "failed to inspect source build environment with {program}: {error}"
            ))
        })?;
    if !output.status.success() || output.stdout.is_empty() || output.stdout.len() > 64 * 1024 {
        return Err(Error::InvalidReleaseMetadata(format!(
            "{program} did not provide a bounded successful version report"
        )));
    }
    String::from_utf8(output.stdout).map_err(|_| {
        Error::InvalidReleaseMetadata(format!("{program} version report is not UTF-8"))
    })
}

fn environment_entry(identity: &str, value: &[u8]) -> MaterializationDigestEntry {
    MaterializationDigestEntry {
        identity: identity.to_string(),
        digest: Digest::from_sha256(value),
    }
}

pub fn build_and_publish_locked_source_recipe(
    cache_root: &Path,
    locked: &LockedModule,
    release: &ReleaseManifest,
    input_entries: &[MaterializationDigestEntry],
    environment_entries: &[MaterializationDigestEntry],
    builder: &dyn SourceRecipeBuilder,
) -> Result<PreparedDevMaterialization> {
    let recipe = crate::profile::resolve_locked_source_recipe(locked, release)?;
    let inputs = materialization_digest("inputs", input_entries)?;
    let environment = materialization_digest("environment", environment_entries)?;
    let outputs = builder.build(SourceRecipeBuildRequest {
        locked,
        recipe: &recipe,
        inputs: inputs.clone(),
        environment: environment.clone(),
    })?;
    let prepared = prepare_dev_materialization(
        locked,
        inputs,
        environment,
        outputs
            .into_iter()
            .map(|output| MaterializationOutput {
                role: output.role,
                kind: output.kind,
                name: output.name,
                bytes: output.bytes,
                detached_manifest: output.detached_manifest,
            })
            .collect(),
    )?;
    publish_prepared_dev_materialization(cache_root, locked, &prepared)?;
    Ok(prepared)
}
