use std::collections::{BTreeMap, BTreeSet, VecDeque};

use serde::{Deserialize, Serialize};

use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::Error;

/// Canonical additive capability set used by dependency intent, artifact
/// selection, lock replay, and materialization recipes.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct CapabilitySet(Vec<String>);

impl CapabilitySet {
    pub fn normalize(
        values: impl IntoIterator<Item = impl AsRef<str>>,
        scope: &str,
    ) -> Result<Self, Error> {
        let mut values = values
            .into_iter()
            .map(|value| {
                let value = value.as_ref();
                validate_stable_name(value, scope)?;
                Ok(value.to_string())
            })
            .collect::<Result<Vec<_>, Error>>()?;
        values.sort();
        values.dedup();
        if values.len() > crate::MAX_MODULE_METADATA_ENTRIES {
            return Err(Error::ModFileParse(format!(
                "{scope} contains more than {} capabilities",
                crate::MAX_MODULE_METADATA_ENTRIES
            )));
        }
        Ok(Self(values))
    }

    pub fn union<'a>(
        sets: impl IntoIterator<Item = &'a CapabilitySet>,
        scope: &str,
    ) -> Result<Self, Error> {
        Self::normalize(
            sets.into_iter()
                .flat_map(|set| set.0.iter().map(String::as_str)),
            scope,
        )
    }

    pub fn as_slice(&self) -> &[String] {
        &self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn contains(&self, capability: &str) -> bool {
        self.0
            .binary_search_by(|candidate| candidate.as_str().cmp(capability))
            .is_ok()
    }

    pub fn cache_key(&self) -> String {
        self.0.join("+")
    }

    pub fn digest(&self) -> crate::digest::Digest {
        let mut bytes = b"vo-capability-set-v1\0".to_vec();
        for capability in &self.0 {
            bytes.extend_from_slice(capability.as_bytes());
            bytes.push(0);
        }
        crate::digest::Digest::from_sha256(&bytes)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProfileDeclaration {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extends: Option<String>,
    pub capabilities: Vec<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CapabilityDeclaration {
    #[serde(default)]
    pub requires: Vec<String>,
    #[serde(default)]
    pub conflicts: Vec<String>,
    #[serde(default)]
    pub targets: Vec<String>,
    #[serde(default)]
    pub packages: Vec<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CapabilityCatalog {
    declarations: BTreeMap<String, CapabilityDeclaration>,
}

impl CapabilityCatalog {
    pub fn from_declarations(
        declarations: BTreeMap<String, CapabilityDeclaration>,
        scope: &str,
    ) -> Result<Self, Error> {
        if declarations.len() > crate::MAX_MODULE_METADATA_ENTRIES {
            return Err(Error::ModFileParse(format!(
                "{scope} contains more than {} capabilities",
                crate::MAX_MODULE_METADATA_ENTRIES
            )));
        }
        for (name, declaration) in &declarations {
            validate_stable_name(name, &format!("{scope}.{name}"))?;
            CapabilitySet::normalize(&declaration.requires, &format!("{scope}.{name}.requires"))?;
            CapabilitySet::normalize(&declaration.conflicts, &format!("{scope}.{name}.conflicts"))?;
            let mut targets = BTreeSet::new();
            for target in &declaration.targets {
                validate_target(target, &format!("{scope}.{name}"))?;
                if !targets.insert(target) {
                    return Err(Error::ModFileParse(format!(
                        "{scope}.{name}.targets contains duplicate {target:?}"
                    )));
                }
            }
            let mut packages = BTreeSet::new();
            for package in &declaration.packages {
                validate_package_requirement(package, &format!("{scope}.{name}.packages"))?;
                if !packages.insert(package) {
                    return Err(Error::ModFileParse(format!(
                        "{scope}.{name}.packages contains duplicate {package:?}"
                    )));
                }
            }
        }
        Ok(Self { declarations })
    }

    pub fn declarations(&self) -> BTreeMap<String, CapabilityDeclaration> {
        self.declarations.clone()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &CapabilityDeclaration)> {
        self.declarations
            .iter()
            .map(|(name, declaration)| (name.as_str(), declaration))
    }

    pub fn validate_set(
        &self,
        capabilities: &CapabilitySet,
        target: &str,
        scope: &str,
    ) -> Result<(), Error> {
        for capability in capabilities.as_slice() {
            let declaration = self.declarations.get(capability).ok_or_else(|| {
                Error::DependencyGraph(format!(
                    "{scope} requests undeclared capability {capability:?}"
                ))
            })?;
            if !declaration.targets.is_empty()
                && !declaration.targets.iter().any(|allowed| allowed == target)
            {
                return Err(Error::DependencyGraph(format!(
                    "{scope} capability {capability:?} does not support target {target}"
                )));
            }
            for conflict in &declaration.conflicts {
                if capabilities.contains(conflict) {
                    return Err(Error::DependencyGraph(format!(
                        "{scope} combines conflicting capabilities {capability:?} and {conflict:?}"
                    )));
                }
            }
            for required in &declaration.requires {
                if !capabilities.contains(required) {
                    return Err(Error::DependencyGraph(format!(
                        "{scope} capability {capability:?} requires {required:?}"
                    )));
                }
            }
        }
        Ok(())
    }

    pub fn required_for_import(&self, import_path: &str) -> Result<CapabilitySet, Error> {
        CapabilitySet::normalize(
            self.declarations
                .iter()
                .filter_map(|(capability, declaration)| {
                    declaration
                        .packages
                        .iter()
                        .any(|package| {
                            import_path == package
                                || (import_path.starts_with(package)
                                    && import_path.as_bytes().get(package.len()) == Some(&b'/'))
                        })
                        .then_some(capability.as_str())
                }),
            &format!("capabilities required by import {import_path:?}"),
        )
    }

    pub fn validate_imports<'a>(
        &self,
        imports: impl IntoIterator<Item = &'a str>,
        selected: &CapabilitySet,
    ) -> Result<(), Error> {
        for import in imports {
            let required = self.required_for_import(import)?;
            let missing = required
                .as_slice()
                .iter()
                .filter(|capability| !selected.contains(capability))
                .cloned()
                .collect::<Vec<_>>();
            if !missing.is_empty() {
                return Err(Error::DependencyGraph(format!(
                    "import {import:?} requires missing capabilities [{}]",
                    missing.join(", ")
                )));
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DependencyCapabilityRequest {
    pub profile: Option<String>,
    pub capabilities: CapabilitySet,
}

impl DependencyCapabilityRequest {
    pub fn new(
        profile: Option<String>,
        capabilities: impl IntoIterator<Item = impl AsRef<str>>,
        scope: &str,
    ) -> Result<Self, Error> {
        if let Some(profile) = profile.as_deref() {
            validate_stable_name(profile, &format!("{scope}.profile"))?;
        }
        Ok(Self {
            profile,
            capabilities: CapabilitySet::normalize(capabilities, &format!("{scope}.capabilities"))?,
        })
    }

    pub fn is_empty(&self) -> bool {
        self.profile.is_none() && self.capabilities.is_empty()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProfileCatalog {
    profiles: BTreeMap<String, CapabilitySet>,
    default_profile: Option<String>,
}

impl ProfileCatalog {
    pub fn from_declarations(
        declarations: BTreeMap<String, ProfileDeclaration>,
        scope: &str,
    ) -> Result<Self, Error> {
        Self::from_declarations_with_default(declarations, None, scope)
    }

    pub fn from_declarations_with_default(
        declarations: BTreeMap<String, ProfileDeclaration>,
        default_profile: Option<String>,
        scope: &str,
    ) -> Result<Self, Error> {
        if declarations.len() > crate::MAX_MODULE_METADATA_ENTRIES {
            return Err(Error::ModFileParse(format!(
                "{scope} contains more than {} profiles",
                crate::MAX_MODULE_METADATA_ENTRIES
            )));
        }
        for (name, declaration) in &declarations {
            validate_stable_name(name, &format!("{scope}.{name}"))?;
            if let Some(parent) = declaration.extends.as_deref() {
                validate_stable_name(parent, &format!("{scope}.{name}.extends"))?;
                if !declarations.contains_key(parent) {
                    return Err(Error::ModFileParse(format!(
                        "{scope}.{name} extends unknown profile {parent:?}"
                    )));
                }
            }
        }
        let mut profiles = BTreeMap::new();
        for name in declarations.keys() {
            resolve_profile_declaration(
                name,
                &declarations,
                &mut profiles,
                &mut BTreeSet::new(),
                scope,
            )?;
        }
        if let Some(default) = default_profile.as_deref() {
            validate_stable_name(default, &format!("{scope}.default"))?;
            if !profiles.contains_key(default) {
                return Err(Error::ModFileParse(format!(
                    "{scope} default profile {default:?} is not declared"
                )));
            }
        }
        Ok(Self {
            profiles,
            default_profile,
        })
    }

    pub fn declarations(&self) -> BTreeMap<String, ProfileDeclaration> {
        self.profiles
            .iter()
            .map(|(name, capabilities)| {
                (
                    name.clone(),
                    ProfileDeclaration {
                        extends: None,
                        capabilities: capabilities.as_slice().to_vec(),
                    },
                )
            })
            .collect()
    }

    pub fn resolve(
        &self,
        profile: Option<&str>,
        capabilities: &CapabilitySet,
        scope: &str,
    ) -> Result<CapabilitySet, Error> {
        let mut sets = Vec::with_capacity(2);
        let profile = profile.or_else(|| {
            capabilities
                .is_empty()
                .then_some(self.default_profile.as_deref())
                .flatten()
        });
        if let Some(profile) = profile {
            validate_stable_name(profile, scope)?;
            sets.push(self.profiles.get(profile).ok_or_else(|| {
                Error::ModFileParse(format!("{scope} requests unknown profile {profile:?}"))
            })?);
        }
        sets.push(capabilities);
        CapabilitySet::union(sets, scope)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &CapabilitySet)> {
        self.profiles
            .iter()
            .map(|(name, capabilities)| (name.as_str(), capabilities))
    }

    pub fn default_profile(&self) -> Option<&str> {
        self.default_profile.as_deref()
    }

    pub fn validate(&self, scope: &str) -> Result<(), Error> {
        for (name, capabilities) in &self.profiles {
            validate_stable_name(name, &format!("{scope}.{name}"))?;
            if capabilities.is_empty() {
                return Err(Error::ModFileParse(format!(
                    "{scope}.{name}.capabilities must contain at least one capability"
                )));
            }
            CapabilitySet::normalize(
                capabilities.as_slice(),
                &format!("{scope}.{name}.capabilities"),
            )?;
        }
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.profiles.is_empty()
    }
}

fn resolve_profile_declaration(
    name: &str,
    declarations: &BTreeMap<String, ProfileDeclaration>,
    resolved: &mut BTreeMap<String, CapabilitySet>,
    visiting: &mut BTreeSet<String>,
    scope: &str,
) -> Result<CapabilitySet, Error> {
    if let Some(capabilities) = resolved.get(name) {
        return Ok(capabilities.clone());
    }
    if !visiting.insert(name.to_string()) {
        return Err(Error::ModFileParse(format!(
            "{scope}.{name} participates in an extends cycle"
        )));
    }
    let declaration = declarations
        .get(name)
        .expect("profile declarations are frozen during expansion");
    let direct = CapabilitySet::normalize(
        &declaration.capabilities,
        &format!("{scope}.{name}.capabilities"),
    )?;
    let capabilities = if let Some(parent) = declaration.extends.as_deref() {
        let parent = resolve_profile_declaration(parent, declarations, resolved, visiting, scope)?;
        CapabilitySet::union([&parent, &direct], &format!("{scope}.{name}.capabilities"))?
    } else {
        direct
    };
    visiting.remove(name);
    if capabilities.is_empty() {
        return Err(Error::ModFileParse(format!(
            "{scope}.{name}.capabilities must contain at least one capability"
        )));
    }
    resolved.insert(name.to_string(), capabilities.clone());
    Ok(capabilities)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ArtifactRole {
    Logic,
    Asset,
    Render,
    Audio,
    UiLogic,
    UiRenderer,
    SurfaceHost,
    Accessibility,
    Diagnostics,
}

impl ArtifactRole {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Logic => "logic",
            Self::Asset => "asset",
            Self::Render => "render",
            Self::Audio => "audio",
            Self::UiLogic => "ui-logic",
            Self::UiRenderer => "ui-renderer",
            Self::SurfaceHost => "surface-host",
            Self::Accessibility => "accessibility",
            Self::Diagnostics => "diagnostics",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RoleArtifactDeclaration {
    pub role: ArtifactRole,
    pub kind: String,
    pub name: String,
    pub digest: Digest,
    pub sbom: Digest,
    pub capability_manifest: Digest,
    pub provenance: Digest,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ArtifactVariantDeclaration {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub profile: Option<String>,
    #[serde(default)]
    pub capabilities: Vec<String>,
    pub target: String,
    pub toolchain: String,
    pub schema: Digest,
    pub abi: Digest,
    pub vo_graph: Digest,
    pub rust_graph: Digest,
    pub js_graph: Digest,
    pub recipe_graph: Digest,
    pub roles: Vec<RoleArtifactDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedArtifactVariant {
    pub profile: Option<String>,
    pub capabilities: CapabilitySet,
    pub target: String,
    pub toolchain: String,
    pub schema: Digest,
    pub abi: Digest,
    pub vo_graph: Digest,
    pub rust_graph: Digest,
    pub js_graph: Digest,
    pub recipe_graph: Digest,
    pub roles: Vec<RoleArtifactDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceRecipeDeclaration {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub profile: Option<String>,
    #[serde(default)]
    pub capabilities: Vec<String>,
    pub target: String,
    pub toolchain: String,
    pub schema: Digest,
    pub abi: Digest,
    pub vo_graph: Digest,
    pub rust_graph: Digest,
    pub js_graph: Digest,
    pub recipe_graph: Digest,
    pub recipe: Digest,
    #[serde(default)]
    pub vo_packages: Vec<String>,
    #[serde(default)]
    pub cargo_features: Vec<String>,
    #[serde(default)]
    pub js_entrypoints: Vec<String>,
    pub role_outputs: Vec<SourceRoleOutputDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceRoleOutputDeclaration {
    pub role: ArtifactRole,
    pub kind: String,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedSourceRecipe {
    pub capabilities: CapabilitySet,
    pub target: String,
    pub toolchain: String,
    pub schema: Digest,
    pub abi: Digest,
    pub vo_graph: Digest,
    pub rust_graph: Digest,
    pub js_graph: Digest,
    pub recipe_graph: Digest,
    pub recipe: Digest,
    pub vo_packages: Vec<String>,
    pub cargo_features: Vec<String>,
    pub js_entrypoints: Vec<String>,
    pub role_outputs: Vec<SourceRoleOutputDeclaration>,
}

impl ResolvedSourceRecipe {
    fn from_declaration(
        declaration: &SourceRecipeDeclaration,
        profiles: &ProfileCatalog,
        scope: &str,
    ) -> Result<Self, Error> {
        validate_target(&declaration.target, scope)?;
        if declaration.toolchain.trim().is_empty() {
            return Err(Error::ExtManifestParse(format!(
                "{scope}.toolchain must be non-empty"
            )));
        }
        let direct =
            CapabilitySet::normalize(&declaration.capabilities, &format!("{scope}.capabilities"))?;
        let capabilities = profiles.resolve(
            declaration.profile.as_deref(),
            &direct,
            &format!("{scope}.profile"),
        )?;
        if capabilities.is_empty() {
            return Err(Error::ExtManifestParse(format!(
                "{scope} must select at least one capability"
            )));
        }
        let vo_packages =
            normalize_recipe_strings(&declaration.vo_packages, &format!("{scope}.vo_packages"))?;
        let cargo_features = normalize_recipe_strings(
            &declaration.cargo_features,
            &format!("{scope}.cargo_features"),
        )?;
        let js_entrypoints = normalize_recipe_strings(
            &declaration.js_entrypoints,
            &format!("{scope}.js_entrypoints"),
        )?;
        if declaration.role_outputs.is_empty()
            || declaration.role_outputs.len() > crate::MAX_MODULE_ARTIFACTS
        {
            return Err(Error::ExtManifestParse(format!(
                "{scope}.role_outputs must contain 1..={} outputs",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }
        let mut role_outputs = declaration.role_outputs.clone();
        role_outputs.sort();
        for pair in role_outputs.windows(2) {
            if pair[0] == pair[1] {
                return Err(Error::ExtManifestParse(format!(
                    "{scope}.role_outputs contains a duplicate output"
                )));
            }
        }
        for output in &role_outputs {
            validate_stable_name(&output.kind, &format!("{scope}.role_outputs.kind"))?;
            crate::schema::validate_file_name(&output.name).map_err(|detail| {
                Error::ExtManifestParse(format!("{scope}.role_outputs.name: {detail}"))
            })?;
        }
        let expected_recipe = source_recipe_identity(
            &capabilities,
            &declaration.target,
            &declaration.toolchain,
            &declaration.schema,
            &declaration.abi,
            &declaration.vo_graph,
            &declaration.rust_graph,
            &declaration.js_graph,
            &declaration.recipe_graph,
            &vo_packages,
            &cargo_features,
            &js_entrypoints,
            &role_outputs,
        );
        if declaration.recipe != expected_recipe {
            return Err(Error::ExtManifestParse(format!(
                "{scope}.recipe does not authenticate the declared source build inputs"
            )));
        }
        Ok(Self {
            capabilities,
            target: declaration.target.clone(),
            toolchain: declaration.toolchain.clone(),
            schema: declaration.schema.clone(),
            abi: declaration.abi.clone(),
            vo_graph: declaration.vo_graph.clone(),
            rust_graph: declaration.rust_graph.clone(),
            js_graph: declaration.js_graph.clone(),
            recipe_graph: declaration.recipe_graph.clone(),
            recipe: declaration.recipe.clone(),
            vo_packages,
            cargo_features,
            js_entrypoints,
            role_outputs,
        })
    }
}

#[allow(clippy::too_many_arguments)]
pub fn source_recipe_identity(
    capabilities: &CapabilitySet,
    target: &str,
    toolchain: &str,
    schema: &Digest,
    abi: &Digest,
    vo_graph: &Digest,
    rust_graph: &Digest,
    js_graph: &Digest,
    recipe_graph: &Digest,
    vo_packages: &[String],
    cargo_features: &[String],
    js_entrypoints: &[String],
    role_outputs: &[SourceRoleOutputDeclaration],
) -> Digest {
    let mut bytes = b"vo-source-recipe-v1\0".to_vec();
    for value in capabilities
        .as_slice()
        .iter()
        .map(String::as_str)
        .chain([target, toolchain])
        .chain(
            [schema, abi, vo_graph, rust_graph, js_graph, recipe_graph]
                .into_iter()
                .map(Digest::as_str),
        )
        .chain(vo_packages.iter().map(String::as_str))
        .chain(cargo_features.iter().map(String::as_str))
        .chain(js_entrypoints.iter().map(String::as_str))
    {
        bytes.extend_from_slice(value.as_bytes());
        bytes.push(0);
    }
    for output in role_outputs {
        bytes.extend_from_slice(output.role.as_str().as_bytes());
        bytes.push(0);
        bytes.extend_from_slice(output.kind.as_bytes());
        bytes.push(0);
        bytes.extend_from_slice(output.name.as_bytes());
        bytes.push(0);
    }
    Digest::from_sha256(&bytes)
}

pub(crate) fn normalize_recipe_strings(
    values: &[String],
    scope: &str,
) -> Result<Vec<String>, Error> {
    if values.len() > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ExtManifestParse(format!(
            "{scope} contains more than {} entries",
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
    let mut normalized = values.to_vec();
    for value in &normalized {
        if value.is_empty()
            || value.len() > 512
            || value.chars().any(char::is_control)
            || value.trim() != value
        {
            return Err(Error::ExtManifestParse(format!(
                "{scope} contains a non-canonical entry"
            )));
        }
    }
    normalized.sort();
    normalized.dedup();
    if normalized != values {
        return Err(Error::ExtManifestParse(format!(
            "{scope} must be sorted and unique"
        )));
    }
    Ok(normalized)
}

pub fn resolve_source_recipes(
    declarations: &[SourceRecipeDeclaration],
    profiles: &ProfileCatalog,
    scope: &str,
) -> Result<Vec<ResolvedSourceRecipe>, Error> {
    if declarations.len() > crate::MAX_MODULE_ARTIFACTS {
        return Err(Error::ExtManifestParse(format!(
            "{scope} contains more than {} recipes",
            crate::MAX_MODULE_ARTIFACTS
        )));
    }
    let mut resolved = Vec::new();
    let mut identities = BTreeSet::new();
    for (index, declaration) in declarations.iter().enumerate() {
        let recipe = ResolvedSourceRecipe::from_declaration(
            declaration,
            profiles,
            &format!("{scope}[{index}]"),
        )?;
        if !identities.insert((
            recipe.capabilities.clone(),
            recipe.target.clone(),
            recipe.toolchain.clone(),
        )) {
            return Err(Error::ExtManifestParse(format!(
                "{scope}[{index}] duplicates an exact capability/target/toolchain identity"
            )));
        }
        resolved.push(recipe);
    }
    Ok(resolved)
}

pub fn resolve_locked_source_recipe(
    locked: &crate::schema::lockfile::LockedModule,
    release: &crate::schema::manifest::ReleaseManifest,
) -> Result<ResolvedSourceRecipe, Error> {
    let selection = locked.selection.as_ref().ok_or_else(|| {
        Error::InvalidReleaseMetadata(format!(
            "{}@{} has no capability selection",
            locked.path, locked.version
        ))
    })?;
    if selection.mode != crate::schema::lockfile::LockedArtifactMode::SourceRecipe {
        return Err(Error::InvalidReleaseMetadata(format!(
            "{}@{} is not locked to a source recipe",
            locked.path, locked.version
        )));
    }
    let identity = selection.source_recipe.as_ref().ok_or_else(|| {
        Error::InvalidReleaseMetadata("locked source recipe identity is missing".to_string())
    })?;
    let profiles = ProfileCatalog::from_declarations_with_default(
        release.profiles.clone(),
        release.default_profile.clone(),
        "profiles",
    )
    .map_err(|error| Error::InvalidReleaseMetadata(error.to_string()))?;
    let recipes = resolve_source_recipes(&release.source_recipes, &profiles, "source_recipes")
        .map_err(|error| Error::InvalidReleaseMetadata(error.to_string()))?;
    let mut matches = recipes.into_iter().filter(|recipe| {
        recipe.recipe == *identity
            && recipe.capabilities.as_slice() == selection.capabilities
            && recipe.target == selection.target
            && recipe.toolchain == selection.toolchain
            && recipe.schema == selection.schema
            && recipe.abi == selection.abi
            && recipe.vo_graph == selection.vo_graph
            && recipe.rust_graph == selection.rust_graph
            && recipe.js_graph == selection.js_graph
            && recipe.recipe_graph == selection.recipe_graph
    });
    let recipe = matches.next().ok_or_else(|| {
        Error::InvalidReleaseMetadata(format!(
            "release no longer contains the exact locked source recipe for {}@{}",
            locked.path, locked.version
        ))
    })?;
    if matches.next().is_some() {
        return Err(Error::InvalidReleaseMetadata(
            "release contains duplicate exact source recipes".to_string(),
        ));
    }
    Ok(recipe)
}

impl ResolvedArtifactVariant {
    pub fn from_declaration(
        declaration: &ArtifactVariantDeclaration,
        profiles: &ProfileCatalog,
        scope: &str,
    ) -> Result<Self, Error> {
        validate_target(&declaration.target, scope)?;
        if declaration.toolchain.trim().is_empty() {
            return Err(Error::ExtManifestParse(format!(
                "{scope}.toolchain must be non-empty"
            )));
        }
        let direct =
            CapabilitySet::normalize(&declaration.capabilities, &format!("{scope}.capabilities"))?;
        let capabilities = profiles.resolve(
            declaration.profile.as_deref(),
            &direct,
            &format!("{scope}.profile"),
        )?;
        if capabilities.is_empty() {
            return Err(Error::ExtManifestParse(format!(
                "{scope} must select at least one capability"
            )));
        }
        if declaration.roles.is_empty() {
            return Err(Error::ExtManifestParse(format!(
                "{scope}.roles must contain at least one role artifact"
            )));
        }
        if declaration.roles.len() > crate::MAX_MODULE_ARTIFACTS {
            return Err(Error::ExtManifestParse(format!(
                "{scope}.roles contains more than {} artifacts",
                crate::MAX_MODULE_ARTIFACTS
            )));
        }
        let mut roles = declaration.roles.clone();
        roles.sort_by(|left, right| {
            (&left.role, &left.kind, &left.name).cmp(&(&right.role, &right.kind, &right.name))
        });
        let mut identities = BTreeSet::new();
        for role in &roles {
            validate_stable_name(&role.kind, &format!("{scope}.roles.kind"))?;
            crate::schema::validate_file_name(&role.name).map_err(|detail| {
                Error::ExtManifestParse(format!("{scope}.roles.name: {detail}"))
            })?;
            if !identities.insert((role.role.clone(), role.kind.as_str(), role.name.as_str())) {
                return Err(Error::ExtManifestParse(format!(
                    "{scope}.roles contains a duplicate role/kind/name identity"
                )));
            }
        }
        Ok(Self {
            profile: declaration.profile.clone(),
            capabilities,
            target: declaration.target.clone(),
            toolchain: declaration.toolchain.clone(),
            schema: declaration.schema.clone(),
            abi: declaration.abi.clone(),
            vo_graph: declaration.vo_graph.clone(),
            rust_graph: declaration.rust_graph.clone(),
            js_graph: declaration.js_graph.clone(),
            recipe_graph: declaration.recipe_graph.clone(),
            roles,
        })
    }
}

pub fn resolve_artifact_variants(
    declarations: &[ArtifactVariantDeclaration],
    profiles: &ProfileCatalog,
    scope: &str,
) -> Result<Vec<ResolvedArtifactVariant>, Error> {
    if declarations.len() > crate::MAX_MODULE_ARTIFACTS {
        return Err(Error::ExtManifestParse(format!(
            "{scope} contains more than {} variants",
            crate::MAX_MODULE_ARTIFACTS
        )));
    }
    let mut resolved = Vec::new();
    resolved
        .try_reserve(declarations.len())
        .map_err(|_| Error::ExtManifestParse(format!("failed to reserve {scope}")))?;
    let mut identities = BTreeSet::new();
    for (index, declaration) in declarations.iter().enumerate() {
        let variant = ResolvedArtifactVariant::from_declaration(
            declaration,
            profiles,
            &format!("{scope}[{index}]"),
        )?;
        let identity = (
            variant.capabilities.clone(),
            variant.target.clone(),
            variant.toolchain.clone(),
        );
        if !identities.insert(identity) {
            return Err(Error::ExtManifestParse(format!(
                "{scope}[{index}] duplicates an exact capability/target/toolchain identity"
            )));
        }
        resolved.push(variant);
    }
    Ok(resolved)
}

pub fn select_exact_artifact_variant<'a>(
    variants: &'a [ResolvedArtifactVariant],
    capabilities: &CapabilitySet,
    target: &str,
    toolchain: &str,
) -> Result<&'a ResolvedArtifactVariant, Error> {
    let mut matches = variants.iter().filter(|variant| {
        variant.capabilities == *capabilities
            && variant.target == target
            && variant.toolchain == toolchain
    });
    let selected = matches.next().ok_or_else(|| {
        Error::InvalidReleaseMetadata(format!(
            "no exact artifact variant for capabilities [{}], target {target}, toolchain {toolchain}",
            capabilities.as_slice().join(", ")
        ))
    })?;
    if matches.next().is_some() {
        return Err(Error::InvalidReleaseMetadata(format!(
            "multiple artifact variants match capabilities [{}], target {target}, toolchain {toolchain}",
            capabilities.as_slice().join(", ")
        )));
    }
    Ok(selected)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedCapabilityModule {
    pub requested_by: Vec<String>,
    pub capabilities: CapabilitySet,
    pub artifact: Option<ResolvedArtifactVariant>,
    pub source_recipe: Option<ResolvedSourceRecipe>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ResolvedCapabilityGraph {
    pub modules: BTreeMap<ModulePath, ResolvedCapabilityModule>,
}

/// Resolve the additive capability graph after semantic-version selection has
/// frozen one release per module. Requests from every incoming edge are
/// normalized against the target module's own profile catalog and then
/// unioned. Artifact selection accepts an exact set only.
pub fn resolve_capability_graph(
    root: &crate::schema::modfile::ModFile,
    graph: &crate::solver::ResolvedGraph,
    target: &str,
    toolchain: &str,
) -> Result<ResolvedCapabilityGraph, Error> {
    resolve_capability_graph_with_policy(root, graph, target, toolchain, SourceBuildPolicy::Deny)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceBuildPolicy {
    Deny,
    Allow,
}

pub fn resolve_capability_graph_with_policy(
    root: &crate::schema::modfile::ModFile,
    graph: &crate::solver::ResolvedGraph,
    target: &str,
    toolchain: &str,
    source_policy: SourceBuildPolicy,
) -> Result<ResolvedCapabilityGraph, Error> {
    let mut requests = BTreeMap::<ModulePath, Vec<(String, DependencyCapabilityRequest)>>::new();
    let mut pending = VecDeque::new();
    for dependency in &root.dependencies {
        requests
            .entry(dependency.module.clone())
            .or_default()
            .push((
                root.module.to_string(),
                dependency.capability_request.clone(),
            ));
        pending.push_back(dependency.module.clone());
    }

    let mut expanded = BTreeSet::new();
    let mut resolved = BTreeMap::<ModulePath, ResolvedCapabilityModule>::new();
    while let Some(module) = pending.pop_front() {
        let selected = graph.modules.get(&module).ok_or_else(|| {
            Error::DependencyGraph(format!(
                "capability request targets {module}, which is absent from the resolved graph"
            ))
        })?;
        let profiles = ProfileCatalog::from_declarations_with_default(
            selected.manifest.profiles.clone(),
            selected.manifest.default_profile.clone(),
            "profiles",
        )
        .map_err(|error| {
            Error::DependencyGraph(format!("{module}: invalid profile catalog: {error}"))
        })?;
        let capability_catalog = CapabilityCatalog::from_declarations(
            selected.manifest.capabilities.clone(),
            "capabilities",
        )
        .map_err(|error| {
            Error::DependencyGraph(format!("{module}: invalid capability catalog: {error}"))
        })?;
        let module_requests = requests.get(&module).cloned().unwrap_or_default();
        let mut requested_by = module_requests
            .iter()
            .map(|(source, _)| source.clone())
            .collect::<Vec<_>>();
        requested_by.sort();
        requested_by.dedup();
        let mut normalized = Vec::new();
        normalized.try_reserve(module_requests.len()).map_err(|_| {
            Error::DependencyGraph(format!(
                "failed to reserve capability requests for {module}"
            ))
        })?;
        for (source, request) in &module_requests {
            normalized.push(
                profiles
                    .resolve(
                        request.profile.as_deref(),
                        &request.capabilities,
                        &format!("{source} -> {module}"),
                    )
                    .map_err(|error| Error::DependencyGraph(error.to_string()))?,
            );
        }
        let capabilities = CapabilitySet::union(
            normalized.iter(),
            &format!("combined capability requests for {module}"),
        )
        .map_err(|error| Error::DependencyGraph(error.to_string()))?;
        capability_catalog.validate_set(&capabilities, target, &module.to_string())?;

        let artifact_variants = resolve_artifact_variants(
            &selected.manifest.artifact_variants,
            &profiles,
            &format!("{module}.artifact_variants"),
        )
        .map_err(|error| Error::DependencyGraph(error.to_string()))?;
        let artifact = artifact_variants
            .iter()
            .find(|variant| {
                variant.capabilities == capabilities
                    && variant.target == target
                    && variant.toolchain == toolchain
            })
            .cloned();
        let source_recipes = resolve_source_recipes(
            &selected.manifest.source_recipes,
            &profiles,
            &format!("{module}.source_recipes"),
        )
        .map_err(|error| Error::DependencyGraph(error.to_string()))?;
        let source_recipe = if artifact.is_none() && source_policy == SourceBuildPolicy::Allow {
            source_recipes
                .iter()
                .find(|recipe| {
                    recipe.capabilities == capabilities
                        && recipe.target == target
                        && recipe.toolchain == toolchain
                })
                .cloned()
        } else {
            None
        };
        if !capabilities.is_empty() && artifact.is_none() && source_recipe.is_none() {
            return Err(Error::DependencyGraph(format!(
                "{module}: no exact published artifact{} for capabilities [{}], target {target}, toolchain {toolchain}",
                if source_policy == SourceBuildPolicy::Allow {
                    " or permitted source recipe"
                } else {
                    ""
                },
                capabilities.as_slice().join(", ")
            )));
        }
        let changed = resolved
            .get(&module)
            .is_none_or(|current| current.capabilities != capabilities);
        resolved.insert(
            module.clone(),
            ResolvedCapabilityModule {
                requested_by,
                capabilities,
                artifact,
                source_recipe,
            },
        );

        if expanded.insert(module.clone()) {
            for dependency in &selected.manifest.dependencies {
                let request = DependencyCapabilityRequest::new(
                    dependency.profile.clone(),
                    &dependency.capabilities,
                    &format!("{module} -> {}", dependency.module),
                )
                .map_err(|error| Error::DependencyGraph(error.to_string()))?;
                requests
                    .entry(dependency.module.clone())
                    .or_default()
                    .push((module.to_string(), request));
                pending.push_back(dependency.module.clone());
            }
        } else if changed {
            // A new incoming edge can enlarge this module's set. Its outgoing
            // declarations remain static, so no downstream request is widened.
        }
    }

    Ok(ResolvedCapabilityGraph { modules: resolved })
}

pub(crate) fn validate_stable_name(value: &str, scope: &str) -> Result<(), Error> {
    let valid = !value.is_empty()
        && value.len() <= 128
        && value
            .bytes()
            .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'-')
        && value
            .as_bytes()
            .first()
            .is_some_and(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit())
        && value
            .as_bytes()
            .last()
            .is_some_and(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit())
        && !value.contains("--");
    if valid {
        Ok(())
    } else {
        Err(Error::ModFileParse(format!(
            "{scope} must use a canonical lower-kebab-case name"
        )))
    }
}

fn validate_target(value: &str, scope: &str) -> Result<(), Error> {
    if value.is_empty()
        || value.len() > 256
        || !value.bytes().all(|byte| {
            byte.is_ascii_lowercase() || byte.is_ascii_digit() || matches!(byte, b'-' | b'_' | b'.')
        })
    {
        return Err(Error::ExtManifestParse(format!(
            "{scope}.target is not a canonical target identity"
        )));
    }
    Ok(())
}

fn validate_package_requirement(value: &str, scope: &str) -> Result<(), Error> {
    if value.is_empty()
        || value.len() > 512
        || value.starts_with('/')
        || value.ends_with('/')
        || value.split('/').any(|part| {
            part.is_empty()
                || !part.bytes().all(|byte| {
                    byte.is_ascii_lowercase()
                        || byte.is_ascii_digit()
                        || matches!(byte, b'-' | b'_' | b'.')
                })
        })
    {
        return Err(Error::ModFileParse(format!(
            "{scope} contains invalid package prefix {value:?}"
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn profile(extends: Option<&str>, capabilities: &[&str]) -> ProfileDeclaration {
        ProfileDeclaration {
            extends: extends.map(str::to_string),
            capabilities: capabilities
                .iter()
                .map(|capability| (*capability).to_string())
                .collect(),
        }
    }

    #[test]
    fn profile_inheritance_default_and_direct_capabilities_resolve_canonically() {
        let catalog = ProfileCatalog::from_declarations_with_default(
            BTreeMap::from([
                ("core".to_string(), profile(None, &["render-core"])),
                (
                    "full".to_string(),
                    profile(Some("core"), &["audio", "render-3d"]),
                ),
            ]),
            Some("full".to_string()),
            "profiles",
        )
        .unwrap();

        let default = catalog
            .resolve(None, &CapabilitySet::default(), "selection")
            .unwrap();
        assert_eq!(
            default.as_slice(),
            &[
                "audio".to_string(),
                "render-3d".to_string(),
                "render-core".to_string()
            ]
        );

        let explicit = catalog
            .resolve(
                Some("core"),
                &CapabilitySet::normalize(["audio"], "selection").unwrap(),
                "selection",
            )
            .unwrap();
        assert_eq!(
            explicit.as_slice(),
            &["audio".to_string(), "render-core".to_string()]
        );
    }

    #[test]
    fn profile_catalog_rejects_cycles_unknown_defaults_and_empty_profiles() {
        let cycle = BTreeMap::from([
            ("a".to_string(), profile(Some("b"), &["render-core"])),
            ("b".to_string(), profile(Some("a"), &["audio"])),
        ]);
        assert!(ProfileCatalog::from_declarations(cycle, "profiles").is_err());

        let core = BTreeMap::from([("core".to_string(), profile(None, &["render-core"]))]);
        assert!(ProfileCatalog::from_declarations_with_default(
            core,
            Some("missing".to_string()),
            "profiles"
        )
        .is_err());

        let empty = BTreeMap::from([("empty".to_string(), profile(None, &[]))]);
        assert!(ProfileCatalog::from_declarations(empty, "profiles").is_err());
    }

    #[test]
    fn profile_capability_contract_enforces_requires_conflicts_targets_and_imports() {
        let catalog = CapabilityCatalog::from_declarations(
            BTreeMap::from([
                ("headless".to_string(), CapabilityDeclaration::default()),
                (
                    "render-3d".to_string(),
                    CapabilityDeclaration {
                        requires: vec!["render-core".to_string()],
                        conflicts: vec!["headless".to_string()],
                        targets: vec!["aarch64-apple-darwin".to_string()],
                        packages: vec!["voplay/render3d".to_string()],
                    },
                ),
                ("render-core".to_string(), CapabilityDeclaration::default()),
            ]),
            "capabilities",
        )
        .unwrap();

        let valid = CapabilitySet::normalize(["render-3d", "render-core"], "selection").unwrap();
        catalog
            .validate_set(&valid, "aarch64-apple-darwin", "selection")
            .unwrap();
        catalog
            .validate_imports(["voplay/render3d/material"], &valid)
            .unwrap();

        let missing = CapabilitySet::normalize(["render-3d"], "selection").unwrap();
        assert!(catalog
            .validate_set(&missing, "aarch64-apple-darwin", "selection")
            .is_err());
        assert!(catalog
            .validate_imports(
                ["voplay/render3d/material"].into_iter(),
                &CapabilitySet::default()
            )
            .is_err());

        let conflicting =
            CapabilitySet::normalize(["headless", "render-3d", "render-core"], "selection")
                .unwrap();
        assert!(catalog
            .validate_set(&conflicting, "aarch64-apple-darwin", "selection")
            .is_err());
        assert!(catalog
            .validate_set(&valid, "wasm32-unknown-unknown", "selection")
            .is_err());
    }
}
