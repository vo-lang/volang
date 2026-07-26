use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::Error;

const WASM_TARGET: &str = "wasm32-unknown-unknown";

/// Parsed extension metadata declared in `vo.mod`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ExtensionManifest {
    pub name: String,
    pub native: Option<NativeExtensionManifest>,
    pub wasm: Option<WasmExtensionManifest>,
    pub web: Option<WebRuntimeManifest>,
    #[serde(default, deserialize_with = "deserialize_generator_providers")]
    pub generators: Vec<GeneratorProviderManifest>,
    #[serde(default)]
    pub artifacts: Vec<crate::profile::ArtifactVariantDeclaration>,
    #[serde(default, deserialize_with = "deserialize_source_recipes")]
    pub source_recipes: Vec<crate::profile::SourceRecipeDeclaration>,
    /// Local build inputs are deliberately excluded from every serialized
    /// extension/publication view.
    #[serde(skip)]
    pub build: Option<ExtensionBuildManifest>,
    /// Canonical owner parsed from the same complete `vo.mod` generation as
    /// this extension contract. Native hosts must use this retained identity
    /// instead of reopening the manifest after analysis.
    #[serde(skip)]
    pub module_owner: Option<crate::identity::ModulePath>,
    /// Filesystem provenance is runtime context, never wire data.
    #[serde(skip)]
    pub manifest_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct NativeExtensionManifest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub library: Option<String>,
    #[serde(deserialize_with = "deserialize_native_targets")]
    pub targets: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GeneratorProviderManifest {
    pub name: String,
    pub version: String,
    pub schema_kind: String,
    #[serde(deserialize_with = "deserialize_generator_artifacts")]
    pub artifacts: BTreeMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtensionBuildManifest {
    pub native: Option<NativeBuildManifest>,
    pub wasm: Option<WasmBuildManifest>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeBuildManifest {
    Cargo {
        manifest: String,
        package: Option<String>,
    },
    Prebuilt {
        path: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmBuildManifest {
    pub wasm: String,
    pub js: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DeclaredArtifactId {
    pub kind: String,
    pub target: String,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum WasmExtensionKind {
    Standalone,
    Bindgen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum WebProviderRole {
    UiLogic,
    UiRenderer,
    GameLogic,
    GameAsset,
    GameRenderer,
    GameAudio,
    SurfaceHost,
    Accessibility,
    Diagnostics,
}

impl WebProviderRole {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::UiLogic => "ui-logic",
            Self::UiRenderer => "ui-renderer",
            Self::GameLogic => "game-logic",
            Self::GameAsset => "game-asset",
            Self::GameRenderer => "game-renderer",
            Self::GameAudio => "game-audio",
            Self::SurfaceHost => "surface-host",
            Self::Accessibility => "accessibility",
            Self::Diagnostics => "diagnostics",
        }
    }

    fn parse(value: &str) -> Option<Self> {
        Some(match value {
            "ui-logic" => Self::UiLogic,
            "ui-renderer" => Self::UiRenderer,
            "game-logic" => Self::GameLogic,
            "game-asset" => Self::GameAsset,
            "game-renderer" => Self::GameRenderer,
            "game-audio" => Self::GameAudio,
            "surface-host" => Self::SurfaceHost,
            "accessibility" => Self::Accessibility,
            "diagnostics" => Self::Diagnostics,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WasmExtensionManifest {
    pub kind: WasmExtensionKind,
    pub wasm: String,
    pub js: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WebRuntimeManifest {
    pub entry: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub provider_role: Option<WebProviderRole>,
    #[serde(
        default,
        skip_serializing_if = "Vec::is_empty",
        deserialize_with = "deserialize_web_provider_roles"
    )]
    pub provider_roles: Vec<WebProviderRole>,
    #[serde(deserialize_with = "deserialize_metadata_strings")]
    pub capabilities: Vec<String>,
    #[serde(deserialize_with = "deserialize_js_modules")]
    pub js_modules: BTreeMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WebProjectManifest {
    pub entry: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ModMetadata {
    pub web: Option<WebProjectManifest>,
    pub extension: Option<ExtensionManifest>,
}

fn deserialize_native_targets<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_ARTIFACTS,
        "extension native targets",
    )
}

fn deserialize_source_recipes<'de, D>(
    deserializer: D,
) -> Result<Vec<crate::profile::SourceRecipeDeclaration>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum AuthoredSourceRecipe {
        Complete(crate::profile::SourceRecipeDeclaration),
        Derived(DerivedSourceRecipe),
    }

    #[derive(Deserialize)]
    #[serde(deny_unknown_fields)]
    struct DerivedSourceRecipe {
        derive: bool,
        #[serde(default)]
        capabilities: Vec<String>,
        target: String,
        toolchain: String,
        #[serde(default)]
        schema_inputs: Vec<String>,
        #[serde(default)]
        abi_inputs: Vec<String>,
        #[serde(default)]
        vo_packages: Vec<String>,
        #[serde(default)]
        cargo_features: Vec<String>,
        #[serde(default)]
        js_entrypoints: Vec<String>,
        role_outputs: Vec<crate::profile::SourceRoleOutputDeclaration>,
    }

    fn digest_strings(
        domain: &[u8],
        values: impl IntoIterator<Item = String>,
    ) -> crate::digest::Digest {
        let mut bytes = domain.to_vec();
        for value in values {
            bytes.extend_from_slice(value.as_bytes());
            bytes.push(0);
        }
        crate::digest::Digest::from_sha256(&bytes)
    }

    let authored = Vec::<AuthoredSourceRecipe>::deserialize(deserializer)?;
    if authored.len() > crate::MAX_MODULE_ARTIFACTS {
        return Err(serde::de::Error::custom(format!(
            "extension source recipes contain more than {} entries",
            crate::MAX_MODULE_ARTIFACTS,
        )));
    }
    authored
        .into_iter()
        .map(|recipe| match recipe {
            AuthoredSourceRecipe::Complete(recipe) => Ok(recipe),
            AuthoredSourceRecipe::Derived(recipe) => {
                if !recipe.derive {
                    return Err(serde::de::Error::custom(
                        "derived source recipe must set derive = true",
                    ));
                }
                let capabilities = crate::profile::CapabilitySet::normalize(
                    &recipe.capabilities,
                    "extension.source_recipes.capabilities",
                )
                .map_err(serde::de::Error::custom)?;
                let vo_packages = crate::profile::normalize_recipe_strings(
                    &recipe.vo_packages,
                    "extension.source_recipes.vo_packages",
                )
                .map_err(serde::de::Error::custom)?;
                let cargo_features = crate::profile::normalize_recipe_strings(
                    &recipe.cargo_features,
                    "extension.source_recipes.cargo_features",
                )
                .map_err(serde::de::Error::custom)?;
                let js_entrypoints = crate::profile::normalize_recipe_strings(
                    &recipe.js_entrypoints,
                    "extension.source_recipes.js_entrypoints",
                )
                .map_err(serde::de::Error::custom)?;
                let schema_inputs = crate::profile::normalize_recipe_strings(
                    &recipe.schema_inputs,
                    "extension.source_recipes.schema_inputs",
                )
                .map_err(serde::de::Error::custom)?;
                let abi_inputs = crate::profile::normalize_recipe_strings(
                    &recipe.abi_inputs,
                    "extension.source_recipes.abi_inputs",
                )
                .map_err(serde::de::Error::custom)?;
                let schema = digest_strings(b"vo-source-schema-v1\0", schema_inputs);
                let vo_graph =
                    digest_strings(b"vo-source-vo-graph-v1\0", vo_packages.iter().cloned());
                let rust_graph =
                    digest_strings(b"vo-source-rust-graph-v1\0", cargo_features.iter().cloned());
                let js_graph =
                    digest_strings(b"vo-source-js-graph-v1\0", js_entrypoints.iter().cloned());
                let abi = digest_strings(
                    b"vo-source-abi-v1\0",
                    abi_inputs
                        .into_iter()
                        .chain(recipe.role_outputs.iter().map(|output| {
                            format!("{}:{}:{}", output.role.as_str(), output.kind, output.name,)
                        })),
                );
                let recipe_graph = digest_strings(
                    b"vo-source-recipe-graph-v1\0",
                    [
                        schema.as_str().to_string(),
                        abi.as_str().to_string(),
                        vo_graph.as_str().to_string(),
                        rust_graph.as_str().to_string(),
                        js_graph.as_str().to_string(),
                    ],
                );
                let identity = crate::profile::source_recipe_identity(
                    &capabilities,
                    &recipe.target,
                    &recipe.toolchain,
                    &schema,
                    &abi,
                    &vo_graph,
                    &rust_graph,
                    &js_graph,
                    &recipe_graph,
                    &vo_packages,
                    &cargo_features,
                    &js_entrypoints,
                    &recipe.role_outputs,
                );
                Ok(crate::profile::SourceRecipeDeclaration {
                    profile: None,
                    capabilities: capabilities.as_slice().to_vec(),
                    target: recipe.target,
                    toolchain: recipe.toolchain,
                    schema,
                    abi,
                    vo_graph,
                    rust_graph,
                    js_graph,
                    recipe_graph,
                    recipe: identity,
                    vo_packages,
                    cargo_features,
                    js_entrypoints,
                    role_outputs: recipe.role_outputs,
                })
            }
        })
        .collect()
}

fn parse_source_recipes_value(
    value: toml::Value,
) -> Result<Vec<crate::profile::SourceRecipeDeclaration>, toml::de::Error> {
    #[derive(Deserialize)]
    struct SourceRecipeList {
        #[serde(deserialize_with = "deserialize_source_recipes")]
        recipes: Vec<crate::profile::SourceRecipeDeclaration>,
    }

    let mut wrapper = toml::value::Table::new();
    wrapper.insert("recipes".to_string(), value);
    toml::Value::Table(wrapper)
        .try_into()
        .map(|parsed: SourceRecipeList| parsed.recipes)
}

fn deserialize_metadata_strings<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_METADATA_ENTRIES,
        "extension metadata string array",
    )
}

fn deserialize_web_provider_roles<'de, D>(deserializer: D) -> Result<Vec<WebProviderRole>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_METADATA_ENTRIES,
        "extension web provider roles",
    )
}

fn deserialize_js_modules<'de, D>(deserializer: D) -> Result<BTreeMap<String, String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_btree_map(
        deserializer,
        crate::MAX_MODULE_METADATA_ENTRIES,
        "extension web JavaScript modules",
    )
}

fn deserialize_generator_providers<'de, D>(
    deserializer: D,
) -> Result<Vec<GeneratorProviderManifest>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_vec(
        deserializer,
        crate::MAX_MODULE_ARTIFACTS,
        "extension generator providers",
    )
}

fn deserialize_generator_artifacts<'de, D>(
    deserializer: D,
) -> Result<BTreeMap<String, String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::schema::deserialize_bounded_btree_map(
        deserializer,
        crate::MAX_MODULE_ARTIFACTS,
        "extension generator artifacts",
    )
}

impl WebRuntimeManifest {
    pub fn js_module_path(&self, name: &str) -> Option<&str> {
        self.js_modules.get(name).map(|path| path.as_str())
    }

    pub fn effective_provider_roles(&self) -> Vec<WebProviderRole> {
        if self.provider_roles.is_empty() {
            self.provider_role.into_iter().collect()
        } else {
            self.provider_roles.clone()
        }
    }
}

impl ExtensionManifest {
    /// Revalidate every invariant normally established by the `vo.mod`
    /// parser. This is required at public typed-data boundaries because these
    /// metadata structs remain constructible by downstream Rust callers and
    /// by serde.
    pub fn validate(&self) -> Result<(), Error> {
        let mut budget = MetadataStringBudget::default();
        let mut module_files = crate::schema::PortablePathSet::default();
        validate_web_runtime_module_name(&self.name, "[extension].name")?;
        budget.add(&self.name, "[extension].name")?;

        if let Some(native) = &self.native {
            if native.targets.is_empty() {
                return Err(Error::ExtManifestParse(
                    "[extension.native].targets must contain at least one target".to_string(),
                ));
            }
            if native.targets.len() > crate::MAX_MODULE_ARTIFACTS {
                return Err(Error::ExtManifestParse(format!(
                    "[extension.native].targets contains more than {} entries",
                    crate::MAX_MODULE_ARTIFACTS
                )));
            }
            let mut targets = BTreeSet::new();
            let library_stem = native.library.as_deref().unwrap_or(&self.name);
            if native.library.is_some() {
                validate_native_library_stem(library_stem)?;
                budget.add(library_stem, "[extension.native].library")?;
            }
            for target in &native.targets {
                validate_native_target_triple(target, "[extension.native].targets")?;
                if !targets.insert(target.as_str()) {
                    return Err(Error::ExtManifestParse(format!(
                        "duplicate target in [extension.native].targets: {target}",
                    )));
                }
                let library = native_library_name(library_stem, target)?;
                validate_file_name(&library, "derived native library name")?;
                budget.add(target, "[extension.native].targets")?;
            }
        }

        if let Some(wasm) = &self.wasm {
            validate_file_name(&wasm.wasm, "[extension.wasm].wasm")?;
            budget.add(&wasm.wasm, "[extension.wasm].wasm")?;
            match wasm.kind {
                WasmExtensionKind::Standalone => {
                    if wasm.js.is_some() {
                        return Err(Error::ExtManifestParse(
                            "'js' is valid only for bindgen [extension.wasm]".to_string(),
                        ));
                    }
                }
                WasmExtensionKind::Bindgen => {
                    let js = wasm.js.as_deref().ok_or_else(|| {
                        Error::ExtManifestParse(
                            "missing 'js' in bindgen [extension.wasm]".to_string(),
                        )
                    })?;
                    validate_file_name(js, "[extension.wasm].js")?;
                }
            }
            if let Some(js) = wasm.js.as_deref() {
                budget.add(js, "[extension.wasm].js")?;
            }
        }

        if let Some(web) = &self.web {
            if let Some(entry) = web.entry.as_deref() {
                validate_normalized_metadata_string(entry, "[extension.web].entry")?;
                budget.add(entry, "[extension.web].entry")?;
            }
            if web.provider_roles.len() > crate::MAX_MODULE_METADATA_ENTRIES {
                return Err(Error::ExtManifestParse(format!(
                    "[extension.web].provider_roles contains more than {} entries",
                    crate::MAX_MODULE_METADATA_ENTRIES
                )));
            }
            let mut provider_roles = BTreeSet::new();
            for role in &web.provider_roles {
                if !provider_roles.insert(*role) {
                    return Err(Error::ExtManifestParse(format!(
                        "duplicate role in [extension.web].provider_roles: {}",
                        role.as_str()
                    )));
                }
            }
            if let Some(primary) = web.provider_role {
                if let Some(first) = web.provider_roles.first() {
                    if *first != primary {
                        return Err(Error::ExtManifestParse(
                            "[extension.web].provider_role must equal the first provider_roles entry"
                                .to_string(),
                        ));
                    }
                }
            } else if !web.provider_roles.is_empty() {
                return Err(Error::ExtManifestParse(
                    "[extension.web].provider_role is required when provider_roles is declared"
                        .to_string(),
                ));
            }
            if web.js_modules.contains_key("renderer") && web.effective_provider_roles().is_empty()
            {
                return Err(Error::ExtManifestParse(
                    "[extension.web] provider roles are required when a renderer module is declared"
                        .to_string(),
                ));
            }
            validate_normalized_string_list(
                &web.capabilities,
                "[extension.web].capabilities",
                &mut budget,
            )?;
            if web.js_modules.len() > crate::MAX_MODULE_METADATA_ENTRIES {
                return Err(Error::ExtManifestParse(format!(
                    "[extension.web].js contains more than {} modules",
                    crate::MAX_MODULE_METADATA_ENTRIES
                )));
            }
            for (name, path) in &web.js_modules {
                validate_web_runtime_module_name(name, "[extension.web].js")?;
                validate_relative_path(path, &format!("[extension.web].js.{name}"))?;
                module_files.insert_file(path).map_err(|detail| {
                    Error::ExtManifestParse(format!(
                        "invalid [extension.web].js.{name} path: {detail}"
                    ))
                })?;
                budget.add(name, "[extension.web].js")?;
                budget.add(path, &format!("[extension.web].js.{name}"))?;
            }
        }

        let mut generator_identities = BTreeSet::new();
        for generator in &self.generators {
            validate_normalized_metadata_string(&generator.name, "[extension.generator].name")?;
            validate_normalized_metadata_string(
                &generator.version,
                "[extension.generator].version",
            )?;
            validate_normalized_metadata_string(
                &generator.schema_kind,
                "[extension.generator].schema_kind",
            )?;
            budget.add(&generator.name, "[extension.generator].name")?;
            budget.add(&generator.version, "[extension.generator].version")?;
            budget.add(&generator.schema_kind, "[extension.generator].schema_kind")?;
            if !generator_identities.insert((
                generator.name.as_str(),
                generator.version.as_str(),
                generator.schema_kind.as_str(),
            )) {
                return Err(Error::ExtManifestParse(format!(
                    "duplicate [extension.generator] identity {}@{} for {}",
                    generator.name, generator.version, generator.schema_kind
                )));
            }
            if generator.artifacts.is_empty() {
                return Err(Error::ExtManifestParse(format!(
                    "[extension.generator] {} must declare at least one target artifact",
                    generator.name
                )));
            }
            for (target, artifact) in &generator.artifacts {
                validate_native_target_triple(target, "[extension.generator].artifacts")?;
                validate_file_name(artifact, "[extension.generator].artifacts")?;
                budget.add(target, "[extension.generator].artifacts")?;
                budget.add(artifact, "[extension.generator].artifacts")?;
            }
        }

        if self.native.is_none()
            && self.wasm.is_none()
            && self.web.is_none()
            && self.generators.is_empty()
            && self.artifacts.is_empty()
            && self.source_recipes.is_empty()
        {
            return Err(Error::ExtManifestParse(
                "[extension] must declare at least one runtime or generator provider".to_string(),
            ));
        }

        if let Some(build) = &self.build {
            if build.native.is_none() && build.wasm.is_none() {
                return Err(Error::ExtManifestParse(
                    "[build] must declare native or wasm inputs".to_string(),
                ));
            }
            if let Some(native_build) = &build.native {
                if self.native.is_none() {
                    return Err(Error::ExtManifestParse(
                        "[build.native] requires [extension.native]".to_string(),
                    ));
                }
                match native_build {
                    NativeBuildManifest::Cargo { manifest, package } => {
                        validate_native_cargo_manifest_path(manifest)?;
                        budget.add(manifest, "[build.native].manifest")?;
                        if let Some(package) = package {
                            validate_cargo_package_name(package)?;
                            budget.add(package, "[build.native].package")?;
                        }
                    }
                    NativeBuildManifest::Prebuilt { path } => {
                        validate_relative_path(path, "[build.native].path")?;
                        budget.add(path, "[build.native].path")?;
                    }
                }
            }
            if let Some(wasm_build) = &build.wasm {
                let public_wasm = self.wasm.as_ref().ok_or_else(|| {
                    Error::ExtManifestParse("[build.wasm] requires [extension.wasm]".to_string())
                })?;
                validate_relative_path(&wasm_build.wasm, "[build.wasm].wasm")?;
                budget.add(&wasm_build.wasm, "[build.wasm].wasm")?;
                match public_wasm.kind {
                    WasmExtensionKind::Standalone => {
                        if wasm_build.js.is_some() {
                            return Err(Error::ExtManifestParse(
                                "[build.wasm].js is valid only for bindgen extensions".to_string(),
                            ));
                        }
                    }
                    WasmExtensionKind::Bindgen => {
                        let js = wasm_build.js.as_deref().ok_or_else(|| {
                            Error::ExtManifestParse(
                                "missing 'js' in [build.wasm] for a bindgen extension".to_string(),
                            )
                        })?;
                        validate_relative_path(js, "[build.wasm].js")?;
                        budget.add(js, "[build.wasm].js")?;
                    }
                }
            }
        }

        let native_artifacts = self
            .native
            .as_ref()
            .map_or(0, |native| native.targets.len());
        let wasm_artifacts = self.wasm.as_ref().map_or(0, |wasm| {
            1usize.saturating_add(usize::from(wasm.js.is_some()))
        });
        let generator_artifacts = self
            .generators
            .iter()
            .map(|generator| generator.artifacts.len())
            .try_fold(0usize, usize::checked_add)
            .ok_or_else(|| Error::ExtManifestParse("artifact count overflow".to_string()))?;
        let artifact_count = native_artifacts
            .checked_add(wasm_artifacts)
            .and_then(|count| count.checked_add(generator_artifacts))
            .ok_or_else(|| Error::ExtManifestParse("artifact count overflow".to_string()))?;
        if artifact_count > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::ExtManifestParse(format!(
                "[extension] declares more than {} target artifacts",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }

        Ok(())
    }

    pub fn native_build(&self) -> Option<&NativeBuildManifest> {
        self.build.as_ref()?.native.as_ref()
    }

    pub fn resolve_native_cargo_manifest_path(
        &self,
        module_root: &Path,
    ) -> Result<Option<PathBuf>, Error> {
        self.validate()?;
        let Some(NativeBuildManifest::Cargo { manifest, .. }) = self.native_build() else {
            return Ok(None);
        };
        Ok(Some(module_root.join(manifest)))
    }

    /// Returns the dedicated top-level tree owned by a local Cargo adapter.
    ///
    /// The module resolver treats this whole tree as opaque native input. Its
    /// contents are authenticated only after analysis reaches the extension.
    pub fn resolve_native_cargo_root_path(
        &self,
        module_root: &Path,
    ) -> Result<Option<PathBuf>, Error> {
        self.validate()?;
        let Some(NativeBuildManifest::Cargo { manifest, .. }) = self.native_build() else {
            return Ok(None);
        };
        let (root, _) = manifest.split_once('/').ok_or_else(|| {
            Error::ExtManifestParse(
                "[build.native].manifest must be nested beneath a dedicated top-level native directory"
                    .to_string(),
            )
        })?;
        Ok(Some(module_root.join(root)))
    }

    pub fn resolve_prebuilt_native_path(
        &self,
        module_root: &Path,
    ) -> Result<Option<PathBuf>, Error> {
        self.validate()?;
        let Some(NativeBuildManifest::Prebuilt { path }) = self.native_build() else {
            return Ok(None);
        };
        Ok(Some(module_root.join(path)))
    }

    pub fn web_runtime(&self) -> Option<&WebRuntimeManifest> {
        self.web.as_ref()
    }

    pub fn declared_artifact_ids(&self) -> Vec<DeclaredArtifactId> {
        let mut artifacts = Vec::new();
        if let Some(native) = &self.native {
            let library_stem = native.library.as_deref().unwrap_or(&self.name);
            artifacts.extend(native.targets.iter().map(|target| DeclaredArtifactId {
                kind: "extension-native".to_string(),
                target: target.clone(),
                name: native_library_name_unchecked(library_stem, target),
            }));
        }
        if let Some(wasm) = &self.wasm {
            artifacts.push(DeclaredArtifactId {
                kind: "extension-wasm".to_string(),
                target: WASM_TARGET.to_string(),
                name: wasm.wasm.clone(),
            });
            if let Some(js) = &wasm.js {
                artifacts.push(DeclaredArtifactId {
                    kind: "extension-js-glue".to_string(),
                    target: WASM_TARGET.to_string(),
                    name: js.clone(),
                });
            }
        }
        for generator in &self.generators {
            artifacts.extend(
                generator
                    .artifacts
                    .iter()
                    .map(|(target, name)| DeclaredArtifactId {
                        kind: "extension-generator".to_string(),
                        target: target.clone(),
                        name: name.clone(),
                    }),
            );
        }
        artifacts.sort();
        artifacts
    }

    pub fn all_declared_artifact_ids(&self) -> Vec<DeclaredArtifactId> {
        let mut artifacts = self.declared_artifact_ids();
        for variant in &self.artifacts {
            artifacts.extend(variant.roles.iter().map(|role| DeclaredArtifactId {
                kind: role.kind.clone(),
                target: variant.target.clone(),
                name: role.name.clone(),
            }));
        }
        artifacts.sort();
        artifacts.dedup();
        artifacts
    }

    pub fn supports_target(&self, target: &str) -> bool {
        if target == WASM_TARGET && self.wasm.is_some() {
            return true;
        }
        self.native
            .as_ref()
            .map(|native| native.targets.iter().any(|declared| declared == target))
            .unwrap_or(false)
    }

    pub fn declared_native_target(&self, target: &str) -> Option<&str> {
        self.native
            .as_ref()?
            .targets
            .iter()
            .find(|declared| declared.as_str() == target)
            .map(String::as_str)
    }

    pub fn declared_native_library(&self, target: &str) -> Option<String> {
        let target = self.declared_native_target(target)?;
        let library_stem = self
            .native
            .as_ref()
            .and_then(|native| native.library.as_deref())
            .unwrap_or(&self.name);
        Some(native_library_name_unchecked(library_stem, target))
    }
}

pub fn native_library_name(library_stem: &str, target: &str) -> Result<String, Error> {
    validate_native_library_stem(library_stem)?;
    validate_native_target_triple(target, "[extension.native].targets")?;
    Ok(native_library_name_unchecked(library_stem, target))
}

fn native_library_name_unchecked(library_stem: &str, target: &str) -> String {
    let stem = library_stem.replace('-', "_");
    if target.contains("-windows-") {
        format!("{stem}.dll")
    } else if target.contains("-apple-") {
        format!("lib{stem}.dylib")
    } else {
        format!("lib{stem}.so")
    }
}

/// Discover extension metadata from a package directory's `vo.mod`.
pub fn discover_extensions(pkg_root: &Path) -> Result<Vec<ExtensionManifest>, Error> {
    let mod_path = pkg_root.join("vo.mod");
    if !mod_path.exists() {
        return Ok(Vec::new());
    }
    let content = vo_common::vfs::read_text_file(&mod_path)?;
    Ok(
        crate::schema::modfile::ModFile::parse_project_at(&content, &mod_path)?
            .extension
            .into_iter()
            .collect(),
    )
}

/// Parse extension metadata through the complete authoritative `vo.mod`
/// schema. Invalid identity, dependency, publication, web, or unknown root
/// metadata invalidates the complete manifest before extension fields are
/// exposed.
pub fn parse_ext_manifest_content(
    content: &str,
    manifest_path: &Path,
) -> Result<ExtensionManifest, Error> {
    crate::schema::modfile::ModFile::parse_project_at(content, manifest_path)?
        .extension
        .ok_or_else(|| Error::ExtManifestParse("missing [extension] section".to_string()))
}

/// Parse only the metadata tables from a TOML value.
///
/// The complete public `vo.mod` boundary is `ModFile::parse_project_at`; this
/// text-level partial parser exists only for focused unit tests. Schema
/// composition uses `parse_mod_metadata_value` after the root parser has
/// validated identity and allowed keys.
#[cfg(test)]
pub(crate) fn parse_mod_metadata_content(
    content: &str,
    manifest_path: &Path,
) -> Result<ModMetadata, Error> {
    if content.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
        return Err(Error::ExtManifestParse(format!(
            "{} exceeds the {}-byte text limit",
            manifest_path.display(),
            vo_common::vfs::MAX_TEXT_FILE_BYTES
        )));
    }
    let value: toml::Value = toml::from_str(content).map_err(|error| {
        Error::ExtManifestParse(format!("{}: {error}", manifest_path.display()))
    })?;
    parse_mod_metadata_value(&value, manifest_path)
}

pub(crate) fn parse_mod_metadata_value(
    value: &toml::Value,
    manifest_path: &Path,
) -> Result<ModMetadata, Error> {
    let root = value.as_table().ok_or_else(|| {
        Error::ExtManifestParse("vo.mod metadata must be TOML tables".to_string())
    })?;
    let web = parse_web_project_from_value(root)?;
    let mut extension = parse_extension_from_value(value, manifest_path)?;
    let build = parse_build_from_value(root)?;
    match (&mut extension, build) {
        (Some(extension), build) => extension.build = build,
        (None, Some(_)) => {
            return Err(Error::ExtManifestParse(
                "[build] requires an [extension] public contract".to_string(),
            ));
        }
        (None, None) => {}
    }
    if let Some(extension) = &extension {
        extension.validate()?;
    }
    Ok(ModMetadata { web, extension })
}

fn parse_extension_from_value(
    value: &toml::Value,
    manifest_path: &Path,
) -> Result<Option<ExtensionManifest>, Error> {
    let Some(extension_value) = value.get("extension") else {
        return Ok(None);
    };
    let extension = extension_value
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[extension] must be a table".to_string()))?;
    reject_unknown_keys(
        extension,
        &[
            "name",
            "native",
            "wasm",
            "web",
            "generator",
            "artifacts",
            "source_recipes",
        ],
        "[extension]",
    )?;
    let name = required_string(extension, "name", "[extension]")?;
    let native = parse_native_extension_from_value(extension)?;
    let wasm = parse_wasm_extension_from_value(extension)?;
    let web = parse_web_runtime_from_value(extension)?;
    let generators = parse_generator_providers_from_value(extension)?;
    let artifacts = extension
        .get("artifacts")
        .map(|value| {
            value.clone().try_into().map_err(|error| {
                Error::ExtManifestParse(format!("[[extension.artifacts]] parse error: {error}"))
            })
        })
        .transpose()?
        .unwrap_or_default();
    let source_recipes = extension
        .get("source_recipes")
        .map(|value| {
            parse_source_recipes_value(value.clone()).map_err(|error| {
                Error::ExtManifestParse(format!(
                    "[[extension.source_recipes]] parse error: {error}"
                ))
            })
        })
        .transpose()?
        .unwrap_or_default();
    let manifest = ExtensionManifest {
        name,
        native,
        wasm,
        web,
        generators,
        artifacts,
        source_recipes,
        build: None,
        module_owner: None,
        manifest_path: manifest_path.to_path_buf(),
    };
    Ok(Some(manifest))
}

fn parse_generator_providers_from_value(
    extension: &toml::value::Table,
) -> Result<Vec<GeneratorProviderManifest>, Error> {
    let Some(value) = extension.get("generator") else {
        return Ok(Vec::new());
    };
    let providers = value.as_array().ok_or_else(|| {
        Error::ExtManifestParse("[extension.generator] must be an array of tables".to_string())
    })?;
    if providers.len() > crate::MAX_MODULE_ARTIFACTS {
        return Err(Error::ExtManifestParse(format!(
            "[extension.generator] contains more than {} providers",
            crate::MAX_MODULE_ARTIFACTS
        )));
    }
    let mut parsed = Vec::with_capacity(providers.len());
    for provider in providers {
        let table = provider.as_table().ok_or_else(|| {
            Error::ExtManifestParse("each [extension.generator] entry must be a table".to_string())
        })?;
        reject_unknown_keys(
            table,
            &["name", "version", "schema_kind", "artifacts"],
            "[extension.generator]",
        )?;
        let artifacts = table
            .get("artifacts")
            .and_then(toml::Value::as_table)
            .ok_or_else(|| {
                Error::ExtManifestParse(
                    "[extension.generator].artifacts must be a table".to_string(),
                )
            })?;
        let mut artifact_map = BTreeMap::new();
        for (target, value) in artifacts {
            let artifact = value.as_str().ok_or_else(|| {
                Error::ExtManifestParse(format!(
                    "[extension.generator].artifacts.{target} must be a string"
                ))
            })?;
            artifact_map.insert(target.clone(), artifact.to_string());
        }
        parsed.push(GeneratorProviderManifest {
            name: required_string(table, "name", "[extension.generator]")?,
            version: required_string(table, "version", "[extension.generator]")?,
            schema_kind: required_string(table, "schema_kind", "[extension.generator]")?,
            artifacts: artifact_map,
        });
    }
    Ok(parsed)
}

pub fn extension_name_from_content(content: &str) -> Result<String, Error> {
    Ok(parse_ext_manifest_content(content, Path::new("vo.mod"))?.name)
}

fn parse_web_project_from_value(
    root: &toml::value::Table,
) -> Result<Option<WebProjectManifest>, Error> {
    let Some(web) = root.get("web") else {
        return Ok(None);
    };
    let table = web
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[web] must be a table".to_string()))?;
    reject_unknown_keys(table, &["entry"], "[web]")?;
    Ok(Some(WebProjectManifest {
        entry: optional_nonempty_string(table, "entry", "[web]")?,
    }))
}

fn parse_web_runtime_from_value(
    extension: &toml::value::Table,
) -> Result<Option<WebRuntimeManifest>, Error> {
    let Some(web) = extension.get("web") else {
        return Ok(None);
    };
    let table = web
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[extension.web] must be a table".to_string()))?;
    Ok(Some(parse_canonical_web_runtime_table(table)?))
}

fn parse_canonical_web_runtime_table(
    table: &toml::value::Table,
) -> Result<WebRuntimeManifest, Error> {
    reject_unknown_keys(
        table,
        &[
            "entry",
            "provider_role",
            "provider_roles",
            "capabilities",
            "js",
        ],
        "[extension.web]",
    )?;
    let provider_role = optional_nonempty_string(table, "provider_role", "[extension.web]")?
        .map(|value| {
            WebProviderRole::parse(&value).ok_or_else(|| {
                Error::ExtManifestParse(format!("invalid [extension.web].provider_role {value:?}"))
            })
        })
        .transpose()?;
    let provider_roles = parse_string_array(table, "provider_roles", "[extension.web]")?
        .into_iter()
        .map(|value| {
            WebProviderRole::parse(&value).ok_or_else(|| {
                Error::ExtManifestParse(format!(
                    "invalid [extension.web].provider_roles entry {value:?}"
                ))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(WebRuntimeManifest {
        entry: optional_nonempty_string(table, "entry", "[extension.web]")?,
        provider_role,
        provider_roles,
        capabilities: parse_string_array(table, "capabilities", "[extension.web]")?,
        js_modules: parse_web_runtime_js_modules(table, "[extension.web]")?,
    })
}

fn parse_web_runtime_js_modules(
    table: &toml::value::Table,
    scope: &str,
) -> Result<BTreeMap<String, String>, Error> {
    let Some(value) = table.get("js") else {
        return Ok(BTreeMap::new());
    };
    let js_table = value
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse(format!("'js' in {} must be a table", scope)))?;
    if js_table.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{}.js contains more than {} modules",
            scope,
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
    let mut js_modules = BTreeMap::new();
    for (name, value) in js_table {
        validate_web_runtime_module_name(name, &format!("{}.js", scope))?;
        let path = value.as_str().ok_or_else(|| {
            Error::ExtManifestParse(format!("'{}.js.{}' must be a string", scope, name))
        })?;
        validate_relative_path(path, &format!("{}.js.{}", scope, name))?;
        js_modules.insert(name.clone(), path.to_string());
    }
    Ok(js_modules)
}

fn parse_wasm_extension_from_value(
    extension: &toml::value::Table,
) -> Result<Option<WasmExtensionManifest>, Error> {
    let Some(wasm) = extension.get("wasm") else {
        return Ok(None);
    };
    let wasm = wasm
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[extension.wasm] must be a table".to_string()))?;
    reject_unknown_keys(wasm, &["kind", "wasm", "js"], "[extension.wasm]")?;
    Ok(Some(parse_wasm_extension_table(wasm)?))
}

fn parse_wasm_extension_table(table: &toml::value::Table) -> Result<WasmExtensionManifest, Error> {
    let kind = match optional_string(table, "kind", "[extension.wasm]")?.as_deref() {
        Some("standalone") => WasmExtensionKind::Standalone,
        Some("bindgen") => WasmExtensionKind::Bindgen,
        Some(other) => {
            return Err(Error::ExtManifestParse(format!(
                "unsupported [extension.wasm] kind: {}",
                other,
            )));
        }
        None => {
            return Err(Error::ExtManifestParse(
                "missing 'kind' in [extension.wasm]".to_string(),
            ));
        }
    };
    let wasm = required_string(table, "wasm", "[extension.wasm]")?;
    validate_file_name(&wasm, "[extension.wasm].wasm")?;
    let js = optional_string(table, "js", "[extension.wasm]")?;
    match kind {
        WasmExtensionKind::Standalone => {
            if js.is_some() {
                return Err(Error::ExtManifestParse(
                    "'js' is valid only for bindgen [extension.wasm]".to_string(),
                ));
            }
        }
        WasmExtensionKind::Bindgen => {
            let Some(js_name) = js.as_deref() else {
                return Err(Error::ExtManifestParse(
                    "missing 'js' in bindgen [extension.wasm]".to_string(),
                ));
            };
            validate_file_name(js_name, "[extension.wasm].js")?;
        }
    }
    Ok(WasmExtensionManifest { kind, wasm, js })
}

fn parse_native_extension_from_value(
    extension: &toml::value::Table,
) -> Result<Option<NativeExtensionManifest>, Error> {
    let Some(native) = extension.get("native") else {
        return Ok(None);
    };
    let native = native
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[extension.native] must be a table".to_string()))?;
    reject_unknown_keys(native, &["library", "targets"], "[extension.native]")?;
    let library = optional_nonempty_string(native, "library", "[extension.native]")?;
    if let Some(library) = library.as_deref() {
        validate_native_library_stem(library)?;
    }
    let targets = native
        .get("targets")
        .ok_or_else(|| {
            Error::ExtManifestParse("missing 'targets' in [extension.native]".to_string())
        })?
        .as_array()
        .ok_or_else(|| {
            Error::ExtManifestParse("[extension.native].targets must be an array".to_string())
        })?;
    let target_count = targets.len();
    if target_count > crate::MAX_MODULE_ARTIFACTS {
        return Err(Error::ExtManifestParse(format!(
            "[extension.native].targets contains more than {} entries",
            crate::MAX_MODULE_ARTIFACTS
        )));
    }
    if target_count == 0 {
        return Err(Error::ExtManifestParse(
            "[extension.native].targets must contain at least one target".to_string(),
        ));
    }
    let mut seen_targets = BTreeSet::new();
    let mut parsed_targets = Vec::new();
    parsed_targets.try_reserve(target_count).map_err(|_| {
        Error::ExtManifestParse("failed to reserve [extension.native].targets entries".to_string())
    })?;
    for item in targets {
        let target = item.as_str().ok_or_else(|| {
            Error::ExtManifestParse(
                "each entry in [extension.native].targets must be a string".to_string(),
            )
        })?;
        validate_native_target_triple(target, "[extension.native].targets")?;
        if !seen_targets.insert(target.to_string()) {
            return Err(Error::ExtManifestParse(format!(
                "duplicate target in [extension.native].targets: {target}",
            )));
        }
        parsed_targets.push(target.to_string());
    }
    parsed_targets.sort();

    Ok(Some(NativeExtensionManifest {
        library,
        targets: parsed_targets,
    }))
}

fn parse_build_from_value(
    root: &toml::value::Table,
) -> Result<Option<ExtensionBuildManifest>, Error> {
    let Some(build) = root.get("build") else {
        return Ok(None);
    };
    let build = build
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[build] must be a table".to_string()))?;
    reject_unknown_keys(build, &["native", "wasm"], "[build]")?;
    let native = parse_native_build_from_value(build)?;
    let wasm = parse_wasm_build_from_value(build)?;
    if native.is_none() && wasm.is_none() {
        return Err(Error::ExtManifestParse(
            "[build] must declare native or wasm inputs".to_string(),
        ));
    }
    Ok(Some(ExtensionBuildManifest { native, wasm }))
}

fn parse_native_build_from_value(
    build: &toml::value::Table,
) -> Result<Option<NativeBuildManifest>, Error> {
    let Some(native) = build.get("native") else {
        return Ok(None);
    };
    let native = native
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[build.native] must be a table".to_string()))?;
    let kind = required_string(native, "kind", "[build.native]")?;
    match kind.as_str() {
        "cargo" => {
            reject_unknown_keys(native, &["kind", "manifest", "package"], "[build.native]")?;
            let manifest = required_string(native, "manifest", "[build.native]")?;
            validate_native_cargo_manifest_path(&manifest)?;
            let package = optional_nonempty_string(native, "package", "[build.native]")?;
            if let Some(package) = package.as_deref() {
                validate_cargo_package_name(package)?;
            }
            Ok(Some(NativeBuildManifest::Cargo { manifest, package }))
        }
        "prebuilt" => {
            reject_unknown_keys(native, &["kind", "path"], "[build.native]")?;
            let path = required_string(native, "path", "[build.native]")?;
            validate_relative_path(&path, "[build.native].path")?;
            Ok(Some(NativeBuildManifest::Prebuilt { path }))
        }
        other => Err(Error::ExtManifestParse(format!(
            "unsupported [build.native] kind: {other}",
        ))),
    }
}

fn parse_wasm_build_from_value(
    build: &toml::value::Table,
) -> Result<Option<WasmBuildManifest>, Error> {
    let Some(wasm) = build.get("wasm") else {
        return Ok(None);
    };
    let wasm = wasm
        .as_table()
        .ok_or_else(|| Error::ExtManifestParse("[build.wasm] must be a table".to_string()))?;
    reject_unknown_keys(wasm, &["wasm", "js"], "[build.wasm]")?;
    let wasm_path = required_string(wasm, "wasm", "[build.wasm]")?;
    validate_relative_path(&wasm_path, "[build.wasm].wasm")?;
    let js = optional_relative_path(wasm, "js", "[build.wasm]")?;
    Ok(Some(WasmBuildManifest {
        wasm: wasm_path,
        js,
    }))
}

fn reject_unknown_keys(
    table: &toml::value::Table,
    allowed: &[&str],
    scope: &str,
) -> Result<(), Error> {
    if table.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{scope} contains more than {} keys",
            crate::MAX_MODULE_METADATA_ENTRIES,
        )));
    }
    let unknown = crate::summarize_diagnostic_items(
        table
            .keys()
            .filter(|key| !allowed.contains(&key.as_str()))
            .cloned(),
        "unknown key(s)",
    );
    if unknown.is_empty() {
        return Ok(());
    }
    Err(Error::ExtManifestParse(format!(
        "unsupported key(s) in {}: {}",
        scope, unknown,
    )))
}

fn required_string(table: &toml::value::Table, key: &str, scope: &str) -> Result<String, Error> {
    let value = optional_string(table, key, scope)?
        .ok_or_else(|| Error::ExtManifestParse(format!("missing '{}' in {}", key, scope)))?;
    if is_empty_or_unicode_white_space(&value) {
        return Err(Error::ExtManifestParse(format!(
            "'{}' in {} must not be empty",
            key, scope,
        )));
    }
    Ok(value)
}

fn optional_string(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Option<String>, Error> {
    let Some(value) = table.get(key) else {
        return Ok(None);
    };
    let value = value.as_str().ok_or_else(|| {
        Error::ExtManifestParse(format!("'{}' in {} must be a string", key, scope))
    })?;
    Ok(Some(value.to_string()))
}

fn optional_nonempty_string(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Option<String>, Error> {
    let value = optional_string(table, key, scope)?;
    if let Some(value_str) = value.as_deref() {
        if is_empty_or_unicode_white_space(value_str) {
            return Err(Error::ExtManifestParse(format!(
                "'{}' in {} must not be empty",
                key, scope,
            )));
        }
        if !is_normalized_metadata_string(value_str) {
            return Err(Error::ExtManifestParse(format!(
                "'{}' in {} must be a normalized string",
                key, scope,
            )));
        }
    }
    Ok(value)
}

fn optional_relative_path(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Option<String>, Error> {
    let value = optional_string(table, key, scope)?;
    if let Some(value_str) = value.as_deref() {
        validate_relative_path(value_str, &format!("{}.{}", scope, key))?;
    }
    Ok(value)
}

fn parse_string_array(
    table: &toml::value::Table,
    key: &str,
    scope: &str,
) -> Result<Vec<String>, Error> {
    let Some(value) = table.get(key) else {
        return Ok(Vec::new());
    };
    let items = value.as_array().ok_or_else(|| {
        Error::ExtManifestParse(format!("'{}' in {} must be an array", key, scope))
    })?;
    if items.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{}.{} contains more than {} entries",
            scope,
            key,
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
    let mut parsed = Vec::new();
    parsed.try_reserve(items.len()).map_err(|_| {
        Error::ExtManifestParse(format!("failed to reserve entries for {scope}.{key}"))
    })?;
    let mut seen = BTreeSet::new();
    for item in items {
        let value = item.as_str().ok_or_else(|| {
            Error::ExtManifestParse(format!("each entry in {}.{} must be a string", scope, key,))
        })?;
        if is_empty_or_unicode_white_space(value) {
            return Err(Error::ExtManifestParse(format!(
                "entries in {}.{} must not be empty",
                scope, key,
            )));
        }
        if !is_normalized_metadata_string(value) {
            return Err(Error::ExtManifestParse(format!(
                "entries in {}.{} must be normalized strings",
                scope, key,
            )));
        }
        if !seen.insert(value) {
            return Err(Error::ExtManifestParse(format!(
                "duplicate entry in {}.{}: {}",
                scope, key, value,
            )));
        }
        parsed.push(value.to_string());
    }
    Ok(parsed)
}

#[derive(Default)]
struct MetadataStringBudget {
    bytes: usize,
}

impl MetadataStringBudget {
    fn add(&mut self, value: &str, field: &str) -> Result<(), Error> {
        self.bytes = self
            .bytes
            .checked_add(value.len())
            .ok_or_else(|| Error::ExtManifestParse(format!("{field} metadata size overflow")))?;
        if self.bytes > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::ExtManifestParse(format!(
                "[extension] metadata strings exceed the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        Ok(())
    }
}

fn validate_normalized_string_list(
    values: &[String],
    field: &str,
    budget: &mut MetadataStringBudget,
) -> Result<(), Error> {
    if values.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{field} contains more than {} entries",
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
    let mut seen = BTreeSet::new();
    for value in values {
        if is_empty_or_unicode_white_space(value) {
            return Err(Error::ExtManifestParse(format!(
                "entries in {field} must not be empty"
            )));
        }
        if !is_normalized_metadata_string(value) {
            return Err(Error::ExtManifestParse(format!(
                "entries in {field} must be normalized strings"
            )));
        }
        if !seen.insert(value.as_str()) {
            return Err(Error::ExtManifestParse(format!(
                "duplicate entry in {field}: {value}"
            )));
        }
        budget.add(value, field)?;
    }
    Ok(())
}

fn validate_normalized_metadata_string(value: &str, field: &str) -> Result<(), Error> {
    if is_empty_or_unicode_white_space(value) {
        return Err(Error::ExtManifestParse(format!(
            "{field} must not be empty"
        )));
    }
    if !is_normalized_metadata_string(value) {
        return Err(Error::ExtManifestParse(format!(
            "{field} must be a normalized string"
        )));
    }
    Ok(())
}

fn validate_web_runtime_module_name(name: &str, field: &str) -> Result<(), Error> {
    if name.is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "{} must not be empty",
            field,
        )));
    }
    if name
        .chars()
        .any(|ch| !ch.is_ascii_alphanumeric() && ch != '_' && ch != '-')
    {
        return Err(Error::ExtManifestParse(format!(
            "{} must be a normalized identifier",
            field,
        )));
    }
    Ok(())
}

fn validate_relative_path(value: &str, field: &str) -> Result<(), Error> {
    crate::schema::validate_portable_relative_path(value).map_err(|_| {
        Error::ExtManifestParse(format!(
            "{} must be a normalized module-relative path",
            field,
        ))
    })
}

fn validate_native_cargo_manifest_path(value: &str) -> Result<(), Error> {
    validate_relative_path(value, "[build.native].manifest")?;
    if value.rsplit('/').next() != Some("Cargo.toml") {
        return Err(Error::ExtManifestParse(
            "[build.native].manifest must name a Cargo.toml file".to_string(),
        ));
    }
    let Some((native_root, _)) = value.split_once('/') else {
        return Err(Error::ExtManifestParse(
            "[build.native].manifest must be nested beneath a dedicated top-level native directory; that directory is reserved as an opaque native build root"
                .to_string(),
        ));
    };
    let native_root_key = crate::schema::portable_case_key(native_root);
    if matches!(
        native_root_key.as_str(),
        ".git" | ".volang" | ".vo-cache" | "node_modules" | "target"
    ) {
        return Err(Error::ExtManifestParse(format!(
            "[build.native].manifest top-level native directory '{native_root}' conflicts with a reserved module cache or ignored directory",
        )));
    }
    Ok(())
}

fn validate_cargo_package_name(value: &str) -> Result<(), Error> {
    if value.is_empty()
        || !value
            .chars()
            .all(|character| character.is_ascii_alphanumeric() || matches!(character, '-' | '_'))
    {
        return Err(Error::ExtManifestParse(
            "[build.native].package must be a canonical Cargo package name".to_string(),
        ));
    }
    Ok(())
}

fn validate_native_library_stem(value: &str) -> Result<(), Error> {
    validate_web_runtime_module_name(value, "[extension.native].library")?;
    crate::schema::validate_portable_path_component(value).map_err(|_| {
        Error::ExtManifestParse(
            "[extension.native].library must be a portable library stem".to_string(),
        )
    })
}

fn validate_file_name(value: &str, field: &str) -> Result<(), Error> {
    crate::schema::validate_file_name(value)
        .map_err(|_| Error::ExtManifestParse(format!("{} must be a file name, not a path", field)))
}

fn validate_target_triple(value: &str, field: &str) -> Result<(), Error> {
    if value.is_empty() {
        return Err(Error::ExtManifestParse(format!(
            "{} must not be empty",
            field,
        )));
    }
    if crate::schema::validate_portable_path_component(value).is_err() {
        return Err(Error::ExtManifestParse(format!(
            "{} must be a portable path component",
            field,
        )));
    }
    if value.matches('-').count() < 2
        || value.split('-').any(|segment| {
            segment.is_empty()
                || !segment.chars().all(|ch| {
                    ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '_' || ch == '.'
                })
        })
    {
        return Err(Error::ExtManifestParse(format!(
            "{} must be a canonical Rust target triple",
            field,
        )));
    }
    Ok(())
}

fn validate_native_target_triple(value: &str, field: &str) -> Result<(), Error> {
    validate_target_triple(value, field)?;
    if value.starts_with("wasm32-") || value.starts_with("wasm64-") {
        return Err(Error::ExtManifestParse(format!(
            "{field} must contain native host target triples",
        )));
    }
    Ok(())
}

#[inline]
fn is_empty_or_unicode_white_space(value: &str) -> bool {
    value.is_empty()
        || value
            .chars()
            .all(vo_common::identifier::is_unicode_white_space)
}

#[inline]
fn is_normalized_metadata_string(value: &str) -> bool {
    !vo_common::identifier::has_unicode_white_space_boundary(value)
        && !value.chars().any(vo_common::identifier::is_unicode_control)
}

#[cfg(test)]
mod tests {
    use super::*;

    const CARGO_MANIFEST: &str = r#"
format = 1
module = "github.com/acme/demo"
version = "0.1.0"
vo = "1.0.0"

[dependencies]
"github.com/acme/support" = "^1.0.0"

[web]
entry = "main.vo"

[extension]
name = "demo-runtime"

[extension.native]
library = "vo_demo"
targets = ["x86_64-unknown-linux-gnu", "aarch64-apple-darwin", "x86_64-pc-windows-msvc"]

[extension.wasm]
kind = "bindgen"
wasm = "demo.wasm"
js = "demo.js"

[extension.web]
entry = "Run"
provider_role = "ui-renderer"
provider_roles = ["ui-renderer"]
capabilities = ["render_surface", "widget"]

[extension.web.js]
renderer = "js/runtime.js"

[build.native]
kind = "cargo"
manifest = "rust/ext/Cargo.toml"
package = "demo-native"

[build.wasm]
wasm = "rust/pkg/demo_bg.wasm"
js = "rust/pkg/demo.js"
"#;

    fn project_manifest(body: &str) -> String {
        format!("format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"1.0.0\"\n\n{body}")
    }

    #[test]
    fn parses_vnext_public_contract_and_local_build_inputs() {
        let metadata =
            parse_mod_metadata_content(CARGO_MANIFEST, Path::new("/tmp/demo/vo.mod")).unwrap();
        assert_eq!(metadata.web.unwrap().entry.as_deref(), Some("main.vo"));
        let extension = metadata.extension.unwrap();
        assert_eq!(extension.name, "demo-runtime");
        assert_eq!(
            extension.native.as_ref().unwrap().library.as_deref(),
            Some("vo_demo")
        );
        assert_eq!(
            extension.native.as_ref().unwrap().targets,
            vec![
                "aarch64-apple-darwin",
                "x86_64-pc-windows-msvc",
                "x86_64-unknown-linux-gnu",
            ]
        );
        assert_eq!(
            extension.wasm.as_ref().unwrap().js.as_deref(),
            Some("demo.js")
        );
        assert_eq!(
            extension
                .resolve_native_cargo_manifest_path(Path::new("/tmp/demo"))
                .unwrap(),
            Some(PathBuf::from("/tmp/demo/rust/ext/Cargo.toml"))
        );
        assert_eq!(
            extension
                .resolve_native_cargo_root_path(Path::new("/tmp/demo"))
                .unwrap(),
            Some(PathBuf::from("/tmp/demo/rust"))
        );
        assert!(matches!(
            extension.native_build(),
            Some(NativeBuildManifest::Cargo { manifest, package })
                if manifest == "rust/ext/Cargo.toml"
                    && package.as_deref() == Some("demo-native")
        ));
        assert_eq!(
            extension
                .declared_native_library("aarch64-apple-darwin")
                .as_deref(),
            Some("libvo_demo.dylib")
        );
    }

    #[test]
    fn extension_serialization_excludes_build_and_filesystem_provenance() {
        let extension =
            parse_ext_manifest_content(CARGO_MANIFEST, Path::new("/private/source/demo/vo.mod"))
                .unwrap();
        let value = serde_json::to_value(&extension).unwrap();
        assert!(value.get("build").is_none());
        assert!(value.get("module_owner").is_none());
        assert!(value.get("manifest_path").is_none());
        assert_eq!(value["wasm"]["kind"], "bindgen");
        let json = serde_json::to_string(&extension).unwrap();
        assert!(!json.contains("rust/ext/Cargo.toml"));
        assert!(!json.contains("/private/source"));

        let public: ExtensionManifest = serde_json::from_str(&json).unwrap();
        assert!(public.build.is_none());
        assert!(public.module_owner.is_none());
        assert!(public.manifest_path.as_os_str().is_empty());
        public.validate().unwrap();

        let mut injected = serde_json::to_value(&extension).unwrap();
        injected["build"] = serde_json::json!({ "native": { "path": "private" } });
        assert!(serde_json::from_value::<ExtensionManifest>(injected).is_err());
    }

    #[test]
    fn root_unknown_keys_are_left_to_the_mod_file_parser() {
        let value: toml::Value = toml::from_str(CARGO_MANIFEST).unwrap();
        let metadata = parse_mod_metadata_value(&value, Path::new("vo.mod")).unwrap();
        assert!(metadata.extension.is_some());
    }

    #[test]
    fn public_extension_parser_validates_the_complete_mod_file() {
        let missing_identity = concat!(
            "format = 1\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
            "[extension]\nname = \"demo\"\n[extension.web]\n",
        );
        let error = parse_ext_manifest_content(missing_identity, Path::new("vo.mod")).unwrap_err();
        assert!(
            error.to_string().contains("missing or non-string 'module'"),
            "{error}"
        );

        let unknown_root =
            project_manifest("unknown = true\n[extension]\nname = \"demo\"\n[extension.web]\n");
        let error = parse_ext_manifest_content(&unknown_root, Path::new("vo.mod")).unwrap_err();
        assert!(
            error.to_string().contains("unknown key 'unknown'"),
            "{error}"
        );
    }

    #[test]
    fn native_library_names_are_derived_from_name_and_target() {
        assert_eq!(
            native_library_name("demo-runtime", "aarch64-apple-darwin").unwrap(),
            "libdemo_runtime.dylib"
        );
        assert_eq!(
            native_library_name("demo-runtime", "x86_64-unknown-linux-gnu").unwrap(),
            "libdemo_runtime.so"
        );
        assert_eq!(
            native_library_name("demo-runtime", "x86_64-pc-windows-msvc").unwrap(),
            "demo_runtime.dll"
        );
    }

    #[test]
    fn prebuilt_native_inputs_are_explicit_and_exact() {
        let manifest = parse_ext_manifest_content(
            &project_manifest(
                r#"
[extension]
name = "demo"

[extension.native]
targets = ["aarch64-apple-darwin"]

[build.native]
kind = "prebuilt"
path = "native/libdemo.dylib"
"#,
            ),
            Path::new("/tmp/demo/vo.mod"),
        )
        .unwrap();
        assert_eq!(
            manifest
                .resolve_prebuilt_native_path(Path::new("/tmp/demo"))
                .unwrap(),
            Some(PathBuf::from("/tmp/demo/native/libdemo.dylib"))
        );
        assert_eq!(
            manifest
                .resolve_native_cargo_manifest_path(Path::new("/tmp/demo"))
                .unwrap(),
            None
        );
    }

    #[test]
    fn native_build_variants_are_closed_and_required() {
        for (content, expected) in [
            (
                r#"
[extension]
name = "demo"
[extension.native]
targets = ["aarch64-apple-darwin"]
[build.native]
kind = "cargo"
"#,
                "missing 'manifest'",
            ),
            (
                r#"
[extension]
name = "demo"
[extension.native]
targets = ["aarch64-apple-darwin"]
[build.native]
kind = "prebuilt"
"#,
                "missing 'path'",
            ),
            (
                r#"
[extension]
name = "demo"
[extension.native]
targets = ["aarch64-apple-darwin"]
[build.native]
kind = "shell"
command = "build.sh"
"#,
                "unsupported [build.native] kind",
            ),
        ] {
            let error = parse_ext_manifest_content(&project_manifest(content), Path::new("vo.mod"))
                .unwrap_err();
            assert!(error.to_string().contains(expected), "{error}");
        }
    }

    #[test]
    fn cargo_build_manifest_requires_a_dedicated_visible_native_root() {
        let manifest_with = |path: &str| {
            project_manifest(&format!(
                concat!(
                    "[extension]\n",
                    "name = \"demo\"\n",
                    "[extension.native]\n",
                    "targets = [\"aarch64-apple-darwin\"]\n",
                    "[build.native]\n",
                    "kind = \"cargo\"\n",
                    "manifest = {:?}\n",
                ),
                path
            ))
        };

        let root_error =
            parse_ext_manifest_content(&manifest_with("Cargo.toml"), Path::new("vo.mod"))
                .unwrap_err();
        assert!(
            root_error
                .to_string()
                .contains("dedicated top-level native directory"),
            "{root_error}",
        );

        for ignored in [
            ".git",
            ".GIT",
            ".volang",
            ".VOLANG",
            ".vo-cache",
            ".VO-CACHE",
            "node_modules",
            "NODE_MODULES",
            "target",
            "Target",
        ] {
            let error = parse_ext_manifest_content(
                &manifest_with(&format!("{ignored}/deep/Cargo.toml")),
                Path::new("vo.mod"),
            )
            .unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains("conflicts with a reserved module cache or ignored directory"),
                "{ignored}: {error}",
            );
        }

        let extension = parse_ext_manifest_content(
            &manifest_with("native/tooling/deep/Cargo.toml"),
            Path::new("vo.mod"),
        )
        .unwrap();
        assert_eq!(
            extension
                .resolve_native_cargo_root_path(Path::new("/tmp/demo"))
                .unwrap(),
            Some(PathBuf::from("/tmp/demo/native")),
        );
    }

    #[test]
    fn build_contracts_require_matching_public_contracts() {
        let native_error = parse_mod_metadata_content(
            r#"
[extension]
name = "demo"
[extension.web]
capabilities = ["widget"]
[build.native]
kind = "cargo"
manifest = "rust/Cargo.toml"
"#,
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(native_error
            .to_string()
            .contains("[build.native] requires [extension.native]"));

        let wasm_error = parse_ext_manifest_content(
            &project_manifest(
                r#"
[extension]
name = "demo"
[extension.wasm]
kind = "standalone"
wasm = "demo.wasm"
[build.wasm]
wasm = "out/demo.wasm"
js = "out/demo.js"
"#,
            ),
            Path::new("vo.mod"),
        )
        .unwrap_err();
        assert!(wasm_error
            .to_string()
            .contains("valid only for bindgen extensions"));
    }

    #[test]
    fn legacy_extension_keys_fail_closed() {
        for content in [
            r#"
[extension]
name = "demo"
include = ["assets"]
"#,
            r#"
[extension]
name = "demo"
[extension.native]
path = "rust/target/debug/libdemo"
targets = ["aarch64-apple-darwin"]
"#,
            r#"
[extension]
name = "demo"
[extension.native]
cargo_manifest = "rust/Cargo.toml"
targets = ["aarch64-apple-darwin"]
"#,
            r#"
[extension]
name = "demo"
[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libdemo.dylib"
"#,
            r#"
[extension]
name = "demo"
[extension.wasm]
type = "standalone"
wasm = "demo.wasm"
"#,
            r#"
[extension]
name = "demo"
[extension.wasm]
kind = "bindgen"
wasm = "demo.wasm"
js_glue = "demo.js"
local_wasm = "dist/demo.wasm"
local_js_glue = "dist/demo.js"
"#,
            r#"
[web]
entry = "main.vo"
include = ["assets"]
[extension]
name = "demo"
[extension.web]
include = ["js"]
capabilities = ["widget"]
"#,
        ] {
            assert!(
                parse_ext_manifest_content(&project_manifest(content), Path::new("vo.mod"),)
                    .is_err()
            );
        }
    }
}
