use crate::ext_manifest::{
    ExtensionManifest, ModMetadata, NativeBuildManifest, WasmExtensionKind, WebProjectManifest,
};
use crate::identity::{ModIdentity, ModulePath};
use crate::profile::{
    CapabilityCatalog, CapabilityDeclaration, DependencyCapabilityRequest, ProfileCatalog,
    ProfileDeclaration,
};
use crate::version::{DepConstraint, ExactVersion, ToolchainConstraint};
use crate::Error;
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

/// Parsed representation of a `vo.mod` file.
///
/// The root `module` is either a host-qualified public ModuleId or a reserved
/// `local/<name>` identity for an unpublished workspace/bundle module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModFile {
    pub format: u64,
    pub module: ModIdentity,
    pub version: ExactVersion,
    pub vo: ToolchainConstraint,
    pub capabilities: CapabilityCatalog,
    pub profiles: ProfileCatalog,
    pub dependencies: Vec<Dependency>,
    pub web: Option<WebProjectManifest>,
    pub extension: Option<ExtensionManifest>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub module: ModulePath,
    pub constraint: DepConstraint,
    pub capability_request: DependencyCapabilityRequest,
}

impl ModFile {
    /// Parse an ordinary on-disk `vo.mod` file.
    ///
    /// Projects use either a public host-qualified ModuleId or an unpublished
    /// `local/*` ModuleId. Toolchain-synthesized single-file manifests use
    /// [`Self::parse_ephemeral`] for their narrower schema.
    pub fn parse(content: &str) -> Result<Self, Error> {
        Self::parse_project(content)
    }

    /// Parse an ordinary project manifest.
    pub fn parse_project(content: &str) -> Result<Self, Error> {
        Self::parse_project_at(content, Path::new("vo.mod"))
    }

    /// Parse an ordinary project manifest and retain its source path in local
    /// extension build metadata.
    pub fn parse_project_at(content: &str, manifest_path: &Path) -> Result<Self, Error> {
        Self::parse_with_root_keys(
            content,
            &[
                "format",
                "module",
                "version",
                "vo",
                "default_profile",
                "capabilities",
                "profiles",
                "dependencies",
                "web",
                "extension",
                "build",
            ],
            "vo.mod",
            manifest_path,
            ModFileIdentityPolicy::Project,
        )
    }

    /// Parse a toolchain-synthesized ephemeral manifest.
    ///
    /// This entry point accepts only `local/<name>` identity and rejects
    /// project-only publication, web, extension, and build sections.
    pub fn parse_ephemeral(content: &str) -> Result<Self, Error> {
        Self::parse_ephemeral_at(content, Path::new("ephemeral vo.mod"))
    }

    /// Parse a toolchain-synthesized ephemeral manifest with source context.
    pub fn parse_ephemeral_at(content: &str, manifest_path: &Path) -> Result<Self, Error> {
        Self::parse_with_root_keys(
            content,
            &["format", "module", "version", "vo"],
            "ephemeral vo.mod",
            manifest_path,
            ModFileIdentityPolicy::Ephemeral,
        )
    }

    /// Parse the restricted `vo.mod` schema embedded in a single source file.
    ///
    /// Inline metadata shares identity and toolchain parsing with `vo.mod`.
    /// Single-file modules are standard-library-only.
    pub(crate) fn parse_inline(content: &str) -> Result<Self, Error> {
        Self::parse_with_root_keys(
            content,
            &["format", "module", "version", "vo"],
            "inline vo.mod",
            Path::new("inline vo.mod"),
            ModFileIdentityPolicy::Ephemeral,
        )
    }

    fn parse_with_root_keys(
        content: &str,
        allowed_root_keys: &[&str],
        scope: &str,
        manifest_path: &Path,
        identity_policy: ModFileIdentityPolicy,
    ) -> Result<Self, Error> {
        if content.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::ModFileParse(format!(
                "vo.mod exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        let value: toml::Value = content
            .parse()
            .map_err(|error| Error::ModFileParse(format!("TOML parse error: {error}")))?;
        let root = value
            .as_table()
            .ok_or_else(|| Error::ModFileParse("vo.mod root must be a TOML table".to_string()))?;
        reject_unknown_keys(root, allowed_root_keys, scope)?;

        let format = root
            .get("format")
            .and_then(toml::Value::as_integer)
            .ok_or_else(|| Error::ModFileParse("missing or non-integer 'format'".to_string()))?;
        if format != 1 {
            return Err(Error::ModFileParse(format!(
                "unsupported vo.mod format: {format}"
            )));
        }

        let module = required_root_string(root, "module")?;
        let module = ModIdentity::parse(module)
            .map_err(|error| Error::ModFileParse(format!("module: {error}")))?;
        identity_policy.validate(&module, scope)?;
        let version = ExactVersion::parse(required_root_string(root, "version")?)
            .map_err(|error| Error::ModFileParse(format!("version: {error}")))?;
        let vo = required_root_string(root, "vo")?;
        let vo = ToolchainConstraint::parse(vo)
            .map_err(|error| Error::ModFileParse(format!("vo: {error}")))?;
        let dependencies = parse_dependencies(root, &module)?;
        let capabilities = parse_capabilities(root)?;
        let profiles = parse_profiles(root)?;
        let ModMetadata { web, mut extension } =
            crate::ext_manifest::parse_mod_metadata_value(&value, manifest_path)?;
        if let Some(extension) = extension.as_mut() {
            extension.module_owner = Some(ModulePath::parse(module.as_str())?);
        }

        let manifest = Self {
            format: 1,
            module,
            version,
            vo,
            capabilities,
            profiles,
            dependencies,
            web,
            extension,
        };
        manifest.validate()?;
        Ok(manifest)
    }

    /// Validate every invariant normally established by parsing `vo.mod`.
    pub fn validate(&self) -> Result<(), Error> {
        if self.format != 1 {
            return Err(Error::ModFileParse(format!(
                "unsupported vo.mod format: {}",
                self.format
            )));
        }
        self.version
            .validate()
            .map_err(|detail| Error::ModFileParse(format!("version: {detail}")))?;
        CapabilityCatalog::from_declarations(self.capabilities.declarations(), "[capabilities]")?;
        self.profiles.validate("[profiles]")?;
        let declared_capabilities = self.capabilities.declarations();
        for (profile, capabilities) in self.profiles.iter() {
            for capability in capabilities.as_slice() {
                if !declared_capabilities.contains_key(capability) {
                    return Err(Error::ModFileParse(format!(
                        "[profiles.{profile}] uses undeclared capability {capability:?}"
                    )));
                }
            }
        }
        if let Some(module) = self.module.as_public() {
            if !module.accepts_version(&self.version) {
                return Err(Error::ModFileParse(format!(
                    "version {} is incompatible with module path {}",
                    self.version, module
                )));
            }
        }
        if self.dependencies.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::ModFileParse(format!(
                "dependencies contains more than {} modules",
                crate::MAX_MODULE_DEPENDENCIES
            )));
        }
        let mut modules = BTreeSet::new();
        for (index, dependency) in self.dependencies.iter().enumerate() {
            if !modules.insert(&dependency.module) {
                return Err(Error::ModFileParse(format!(
                    "duplicate dependency for {}",
                    dependency.module
                )));
            }
            if self.module.as_str() == dependency.module.as_str() {
                return Err(Error::ModFileParse(format!(
                    "module {} must not depend on itself",
                    dependency.module
                )));
            }
            let lower_bound =
                crate::version::ExactVersion::from_semver(dependency.constraint.version.clone());
            if !dependency.module.accepts_version(&lower_bound) {
                return Err(Error::ModFileParse(format!(
                    "dependencies[{index}] constraint {} is incompatible with module path {}",
                    dependency.constraint, dependency.module
                )));
            }
            if let Some(profile) = dependency.capability_request.profile.as_deref() {
                crate::profile::CapabilitySet::normalize(
                    [profile],
                    &format!("dependencies[{index}].profile"),
                )?;
            }
        }
        if let Some(web) = &self.web {
            validate_web_project_manifest(web)?;
        }
        if let Some(extension) = &self.extension {
            extension.validate()?;
            crate::profile::resolve_artifact_variants(
                &extension.artifacts,
                &self.profiles,
                "[[extension.artifacts]]",
            )?;
            crate::profile::resolve_source_recipes(
                &extension.source_recipes,
                &self.profiles,
                "[[extension.source_recipes]]",
            )?;
        }
        Ok(())
    }

    /// Render the canonical `vo.mod` text.
    /// Canonical format: root identity, dependency intent, public runtime
    /// contract, then local build adapters.
    pub fn render(&self) -> Result<String, Error> {
        self.validate()?;
        let mut out = super::BoundedTextOutput::new(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(Error::ModFileParse)?;
        macro_rules! push {
            ($value:expr) => {
                out.push_str($value).map_err(Error::ModFileParse)?
            };
        }
        macro_rules! quoted {
            ($value:expr) => {
                out.push_toml_string($value).map_err(Error::ModFileParse)?
            };
        }

        push!("format = 1\nmodule = ");
        quoted!(self.module.as_str());
        push!("\nversion = ");
        quoted!(&self.version.to_string());
        push!("\nvo = ");
        quoted!(&self.vo.to_string());
        push!("\n");
        if let Some(default_profile) = self.profiles.default_profile() {
            push!("default_profile = ");
            quoted!(default_profile);
            push!("\n");
        }
        for (name, declaration) in self.capabilities.iter() {
            push!("\n[capabilities.");
            push!(name);
            push!("]\n");
            if !declaration.conflicts.is_empty() {
                push!("conflicts = [");
                append_string_array(&mut out, &declaration.conflicts)?;
                push!("]\n");
            }
            if !declaration.requires.is_empty() {
                push!("requires = [");
                append_string_array(&mut out, &declaration.requires)?;
                push!("]\n");
            }
            if !declaration.targets.is_empty() {
                push!("targets = [");
                append_string_array(&mut out, &declaration.targets)?;
                push!("]\n");
            }
            if !declaration.packages.is_empty() {
                push!("packages = [");
                append_string_array(&mut out, &declaration.packages)?;
                push!("]\n");
            }
        }
        for (name, capabilities) in self.profiles.iter() {
            push!("\n[profiles.");
            push!(name);
            push!("]\ncapabilities = [");
            append_string_array(&mut out, capabilities.as_slice())?;
            push!("]\n");
        }
        if !self.dependencies.is_empty() {
            push!("\n[dependencies]\n");
            let mut sorted: Vec<&Dependency> = self.dependencies.iter().collect();
            sorted.sort_by(|left, right| left.module.cmp(&right.module));
            for dependency in sorted {
                quoted!(dependency.module.as_str());
                push!(" = ");
                if dependency.capability_request.is_empty() {
                    quoted!(&dependency.constraint.to_string());
                } else {
                    push!("{ version = ");
                    quoted!(&dependency.constraint.to_string());
                    if let Some(profile) = dependency.capability_request.profile.as_deref() {
                        push!(", profile = ");
                        quoted!(profile);
                    }
                    if !dependency.capability_request.capabilities.is_empty() {
                        push!(", capabilities = [");
                        append_string_array(
                            &mut out,
                            dependency.capability_request.capabilities.as_slice(),
                        )?;
                        push!("]");
                    }
                    push!(" }");
                }
                push!("\n");
            }
        }
        if let Some(web) = &self.web {
            push!("\n[web]\n");
            if let Some(entry) = &web.entry {
                push!("entry = ");
                quoted!(entry);
                push!("\n");
            }
        }
        if let Some(extension) = &self.extension {
            push!("\n[extension]\nname = ");
            quoted!(&extension.name);
            push!("\n");
            if let Some(native) = &extension.native {
                push!("\n[extension.native]\n");
                if let Some(library) = &native.library {
                    push!("library = ");
                    quoted!(library);
                    push!("\n");
                }
                push!("targets = [");
                append_string_array(&mut out, &native.targets)?;
                push!("]\n");
            }
            if let Some(wasm) = &extension.wasm {
                push!("\n[extension.wasm]\nkind = \"");
                push!(match wasm.kind {
                    WasmExtensionKind::Standalone => "standalone",
                    WasmExtensionKind::Bindgen => "bindgen",
                });
                push!("\"\nwasm = ");
                quoted!(&wasm.wasm);
                push!("\n");
                if let Some(js) = &wasm.js {
                    push!("js = ");
                    quoted!(js);
                    push!("\n");
                }
            }
            if let Some(web_runtime) = &extension.web {
                push!("\n[extension.web]\n");
                if let Some(entry) = &web_runtime.entry {
                    push!("entry = ");
                    quoted!(entry);
                    push!("\n");
                }
                if let Some(provider_role) = web_runtime.provider_role {
                    push!("provider_role = \"");
                    push!(provider_role.as_str());
                    push!("\"\n");
                }
                if !web_runtime.provider_roles.is_empty() {
                    push!("provider_roles = [");
                    for (index, role) in web_runtime.provider_roles.iter().enumerate() {
                        if index != 0 {
                            push!(", ");
                        }
                        quoted!(role.as_str());
                    }
                    push!("]\n");
                }
                if !web_runtime.capabilities.is_empty() {
                    push!("capabilities = [");
                    append_string_array(&mut out, &web_runtime.capabilities)?;
                    push!("]\n");
                }
                if !web_runtime.js_modules.is_empty() {
                    push!("\n[extension.web.js]\n");
                    for (name, path) in &web_runtime.js_modules {
                        push!(name);
                        push!(" = ");
                        quoted!(path);
                        push!("\n");
                    }
                }
            }
            for generator in &extension.generators {
                push!("\n[[extension.generator]]\nname = ");
                quoted!(&generator.name);
                push!("\nversion = ");
                quoted!(&generator.version);
                push!("\nschema_kind = ");
                quoted!(&generator.schema_kind);
                push!("\nartifacts = { ");
                for (index, (target, artifact)) in generator.artifacts.iter().enumerate() {
                    if index > 0 {
                        push!(", ");
                    }
                    quoted!(target);
                    push!(" = ");
                    quoted!(artifact);
                }
                push!(" }\n");
            }
            for variant in &extension.artifacts {
                push!("\n[[extension.artifacts]]\n");
                if let Some(profile) = variant.profile.as_deref() {
                    push!("profile = ");
                    quoted!(profile);
                    push!("\n");
                }
                if !variant.capabilities.is_empty() {
                    push!("capabilities = [");
                    append_string_array(&mut out, &variant.capabilities)?;
                    push!("]\n");
                }
                push!("target = ");
                quoted!(&variant.target);
                push!("\ntoolchain = ");
                quoted!(&variant.toolchain);
                for (name, digest) in [
                    ("schema", &variant.schema),
                    ("abi", &variant.abi),
                    ("vo_graph", &variant.vo_graph),
                    ("rust_graph", &variant.rust_graph),
                    ("js_graph", &variant.js_graph),
                    ("recipe_graph", &variant.recipe_graph),
                ] {
                    push!("\n");
                    push!(name);
                    push!(" = ");
                    quoted!(digest.as_str());
                }
                push!("\nroles = [");
                for (index, role) in variant.roles.iter().enumerate() {
                    if index > 0 {
                        push!(", ");
                    }
                    push!("{ role = ");
                    quoted!(role.role.as_str());
                    push!(", kind = ");
                    quoted!(&role.kind);
                    push!(", name = ");
                    quoted!(&role.name);
                    push!(", digest = ");
                    quoted!(role.digest.as_str());
                    push!(", sbom = ");
                    quoted!(role.sbom.as_str());
                    push!(", capability_manifest = ");
                    quoted!(role.capability_manifest.as_str());
                    push!(", provenance = ");
                    quoted!(role.provenance.as_str());
                    push!(" }");
                }
                push!("]\n");
            }
            for recipe in &extension.source_recipes {
                push!("\n[[extension.source_recipes]]\n");
                if let Some(profile) = recipe.profile.as_deref() {
                    push!("profile = ");
                    quoted!(profile);
                    push!("\n");
                }
                if !recipe.capabilities.is_empty() {
                    push!("capabilities = [");
                    append_string_array(&mut out, &recipe.capabilities)?;
                    push!("]\n");
                }
                push!("target = ");
                quoted!(&recipe.target);
                push!("\ntoolchain = ");
                quoted!(&recipe.toolchain);
                for (name, digest) in [
                    ("schema", &recipe.schema),
                    ("abi", &recipe.abi),
                    ("vo_graph", &recipe.vo_graph),
                    ("rust_graph", &recipe.rust_graph),
                    ("js_graph", &recipe.js_graph),
                    ("recipe_graph", &recipe.recipe_graph),
                    ("recipe", &recipe.recipe),
                ] {
                    push!("\n");
                    push!(name);
                    push!(" = ");
                    quoted!(digest.as_str());
                }
                for (name, values) in [
                    ("vo_packages", &recipe.vo_packages),
                    ("cargo_features", &recipe.cargo_features),
                    ("js_entrypoints", &recipe.js_entrypoints),
                ] {
                    if !values.is_empty() {
                        push!("\n");
                        push!(name);
                        push!(" = [");
                        append_string_array(&mut out, values)?;
                        push!("]");
                    }
                }
                push!("\nrole_outputs = [");
                for (index, output) in recipe.role_outputs.iter().enumerate() {
                    if index > 0 {
                        push!(", ");
                    }
                    push!("{ role = ");
                    quoted!(output.role.as_str());
                    push!(", kind = ");
                    quoted!(&output.kind);
                    push!(", name = ");
                    quoted!(&output.name);
                    push!(" }");
                }
                push!("]");
                push!("\n");
            }
            if let Some(build) = &extension.build {
                if let Some(native) = &build.native {
                    push!("\n[build.native]\n");
                    match native {
                        NativeBuildManifest::Cargo { manifest, package } => {
                            push!("kind = \"cargo\"\nmanifest = ");
                            quoted!(manifest);
                            push!("\n");
                            if let Some(package) = package {
                                push!("package = ");
                                quoted!(package);
                                push!("\n");
                            }
                        }
                        NativeBuildManifest::Prebuilt { path } => {
                            push!("kind = \"prebuilt\"\npath = ");
                            quoted!(path);
                            push!("\n");
                        }
                    }
                }
                if let Some(wasm) = &build.wasm {
                    push!("\n[build.wasm]\nwasm = ");
                    quoted!(&wasm.wasm);
                    push!("\n");
                    if let Some(js) = &wasm.js {
                        push!("js = ");
                        quoted!(js);
                        push!("\n");
                    }
                }
            }
        }
        let out = out.finish();
        Self::parse_project(&out)?;
        Ok(out)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ModFileIdentityPolicy {
    Project,
    Ephemeral,
}

impl ModFileIdentityPolicy {
    fn validate(self, module: &ModIdentity, scope: &str) -> Result<(), Error> {
        match (self, module) {
            (Self::Project, _) | (Self::Ephemeral, ModIdentity::Local(_)) => Ok(()),
            (Self::Ephemeral, ModIdentity::Public(_)) => Err(Error::ModFileParse(format!(
                "{scope}: ephemeral module identity must use local/<name>"
            ))),
        }
    }
}

fn validate_web_project_manifest(web: &WebProjectManifest) -> Result<(), Error> {
    if let Some(entry) = web.entry.as_deref() {
        crate::schema::validate_portable_relative_path(entry).map_err(|detail| {
            Error::ModFileParse(format!(
                "[web].entry must be a normalized portable module-relative path: {detail}"
            ))
        })?;
        if !entry.ends_with(".vo") {
            return Err(Error::ModFileParse(
                "[web].entry must name a .vo source file".to_string(),
            ));
        }
    }
    Ok(())
}

fn required_root_string<'a>(root: &'a toml::value::Table, key: &str) -> Result<&'a str, Error> {
    root.get(key)
        .and_then(toml::Value::as_str)
        .ok_or_else(|| Error::ModFileParse(format!("vo.mod: missing or non-string '{key}'")))
}

fn reject_unknown_keys(
    table: &toml::value::Table,
    allowed: &[&str],
    scope: &str,
) -> Result<(), Error> {
    for key in table.keys() {
        if !allowed.contains(&key.as_str()) {
            return Err(Error::ModFileParse(format!("{scope}: unknown key '{key}'")));
        }
    }
    Ok(())
}

fn parse_dependencies(
    root: &toml::value::Table,
    module: &ModIdentity,
) -> Result<Vec<Dependency>, Error> {
    let Some(value) = root.get("dependencies") else {
        return Ok(Vec::new());
    };
    let table = value
        .as_table()
        .ok_or_else(|| Error::ModFileParse("[dependencies] must be a TOML table".to_string()))?;
    if table.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::ModFileParse(format!(
            "dependencies contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }
    let mut dependencies = Vec::new();
    dependencies
        .try_reserve(table.len())
        .map_err(|_| Error::ModFileParse("failed to reserve dependencies".to_string()))?;
    for (path, value) in table {
        let (constraint, capability_request) = if let Some(constraint) = value.as_str() {
            (constraint, DependencyCapabilityRequest::default())
        } else {
            let request = value.as_table().ok_or_else(|| {
                Error::ModFileParse(format!(
                    "dependencies.{path}: expected a string or inline request table"
                ))
            })?;
            reject_unknown_keys(
                request,
                &["version", "profile", "capabilities"],
                &format!("dependencies.{path}"),
            )?;
            let constraint = request
                .get("version")
                .and_then(toml::Value::as_str)
                .ok_or_else(|| {
                    Error::ModFileParse(format!(
                        "dependencies.{path}.version must be a string constraint"
                    ))
                })?;
            let profile = request
                .get("profile")
                .map(|value| {
                    value.as_str().map(str::to_string).ok_or_else(|| {
                        Error::ModFileParse(format!("dependencies.{path}.profile must be a string"))
                    })
                })
                .transpose()?;
            let capabilities = request
                .get("capabilities")
                .map(|value| {
                    value.as_array().ok_or_else(|| {
                        Error::ModFileParse(format!(
                            "dependencies.{path}.capabilities must be an array"
                        ))
                    })
                })
                .transpose()?
                .into_iter()
                .flatten()
                .map(|value| {
                    value.as_str().ok_or_else(|| {
                        Error::ModFileParse(format!(
                            "dependencies.{path}.capabilities entries must be strings"
                        ))
                    })
                })
                .collect::<Result<Vec<_>, Error>>()?;
            (
                constraint,
                DependencyCapabilityRequest::new(
                    profile,
                    capabilities,
                    &format!("dependencies.{path}"),
                )?,
            )
        };
        let dependency = ModulePath::parse(path)
            .map_err(|error| Error::ModFileParse(format!("dependencies.{path}: {error}")))?;
        if module.as_str() == dependency.as_str() {
            return Err(Error::ModFileParse(format!(
                "module {module} must not depend on itself"
            )));
        }
        let constraint = DepConstraint::parse(constraint)
            .map_err(|error| Error::ModFileParse(format!("dependencies.{path}: {error}")))?;
        let lower_bound = crate::version::ExactVersion::from_semver(constraint.version.clone());
        if !dependency.accepts_version(&lower_bound) {
            return Err(Error::ModFileParse(format!(
                "dependencies.{path}: constraint {constraint} is incompatible with module path {dependency}"
            )));
        }
        dependencies.push(Dependency {
            module: dependency,
            constraint,
            capability_request,
        });
    }
    dependencies.sort_by(|left, right| left.module.cmp(&right.module));
    Ok(dependencies)
}

fn parse_profiles(root: &toml::value::Table) -> Result<ProfileCatalog, Error> {
    let Some(value) = root.get("profiles") else {
        return Ok(ProfileCatalog::default());
    };
    let table = value
        .as_table()
        .ok_or_else(|| Error::ModFileParse("[profiles] must be a TOML table".to_string()))?;
    let mut declarations = BTreeMap::new();
    for (name, value) in table {
        let profile = value.as_table().ok_or_else(|| {
            Error::ModFileParse(format!("[profiles.{name}] must be a TOML table"))
        })?;
        reject_unknown_keys(
            profile,
            &["extends", "capabilities"],
            &format!("[profiles.{name}]"),
        )?;
        let extends = profile
            .get("extends")
            .map(|value| {
                value.as_str().map(str::to_string).ok_or_else(|| {
                    Error::ModFileParse(format!("[profiles.{name}].extends must be a string"))
                })
            })
            .transpose()?;
        let capabilities = profile
            .get("capabilities")
            .and_then(toml::Value::as_array)
            .ok_or_else(|| {
                Error::ModFileParse(format!("[profiles.{name}].capabilities must be an array"))
            })?
            .iter()
            .map(|value| {
                value.as_str().map(str::to_string).ok_or_else(|| {
                    Error::ModFileParse(format!(
                        "[profiles.{name}].capabilities entries must be strings"
                    ))
                })
            })
            .collect::<Result<Vec<_>, Error>>()?;
        declarations.insert(
            name.clone(),
            ProfileDeclaration {
                extends,
                capabilities,
            },
        );
    }
    let default_profile =
        root.get("default_profile")
            .map(|value| {
                value.as_str().map(str::to_string).ok_or_else(|| {
                    Error::ModFileParse("default_profile must be a string".to_string())
                })
            })
            .transpose()?;
    ProfileCatalog::from_declarations_with_default(declarations, default_profile, "[profiles]")
}

fn parse_capabilities(root: &toml::value::Table) -> Result<CapabilityCatalog, Error> {
    let Some(value) = root.get("capabilities") else {
        return Ok(CapabilityCatalog::default());
    };
    let table = value
        .as_table()
        .ok_or_else(|| Error::ModFileParse("[capabilities] must be a TOML table".to_string()))?;
    let mut declarations = BTreeMap::new();
    for (name, value) in table {
        let declaration: CapabilityDeclaration = value.clone().try_into().map_err(|error| {
            Error::ModFileParse(format!("[capabilities.{name}] parse error: {error}"))
        })?;
        declarations.insert(name.clone(), declaration);
    }
    CapabilityCatalog::from_declarations(declarations, "[capabilities]")
}

fn append_string_array(
    output: &mut super::BoundedTextOutput,
    items: &[String],
) -> Result<(), Error> {
    let mut sorted = Vec::new();
    sorted.try_reserve(items.len()).map_err(|_| {
        Error::ModFileParse("failed to reserve metadata strings for rendering".to_string())
    })?;
    sorted.extend(items);
    let mut items = sorted;
    items.sort();
    for (index, item) in items.into_iter().enumerate() {
        if index != 0 {
            output.push_str(", ").map_err(Error::ModFileParse)?;
        }
        output.push_toml_string(item).map_err(Error::ModFileParse)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_the_minimal_toml_protocol() {
        let content = r#"
# Project identity and toolchain intent.
format = 1
module = "github.com/acme/app"
version = "0.1.0"
vo = "1.0.0"

[dependencies]
"github.com/vo-lang/vogui" = "^0.4.0"
"github.com/acme/http" = "1.3.1"
"#;
        let mf = ModFile::parse(content).unwrap();
        assert_eq!(mf.module.as_str(), "github.com/acme/app");
        assert_eq!(mf.vo.to_string(), "1.0.0");
        assert_eq!(mf.dependencies.len(), 2);
        assert_eq!(mf.dependencies[0].constraint.to_string(), "1.3.1");
    }

    #[test]
    fn project_and_ephemeral_identity_parsers_are_disjoint() {
        let project =
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"1.0.0\"\n";
        let ephemeral =
            "format = 1\nmodule = \"local/demo\"\nversion = \"0.1.0\"\nvo = \"1.0.0\"\n";

        assert!(ModFile::parse_project(project).is_ok());
        assert!(ModFile::parse_project(ephemeral).is_ok());
        assert!(ModFile::parse_ephemeral(ephemeral).is_ok());
        assert!(ModFile::parse_ephemeral(project).is_err());

        let with_project_metadata = format!("{ephemeral}\n[web]\nentry = \"main.vo\"\n");
        let error = ModFile::parse_ephemeral(&with_project_metadata).unwrap_err();
        assert!(error.to_string().contains("unknown key 'web'"), "{error}");

        let with_dependencies =
            format!("{ephemeral}\n[dependencies]\n\"github.com/acme/lib\" = \"^1.0.0\"\n");
        let error = ModFile::parse_ephemeral(&with_dependencies).unwrap_err();
        assert!(
            error.to_string().contains("unknown key 'dependencies'"),
            "{error}"
        );
    }

    #[test]
    fn project_parser_preserves_extension_manifest_path() {
        let content = r#"
format = 1
module = "github.com/acme/app"
version = "0.1.0"
vo = "1.0.0"

[extension]
name = "app"

[extension.web]
"#;
        let path = Path::new("/workspace/app/vo.mod");
        let parsed = ModFile::parse_project_at(content, path).unwrap();
        assert_eq!(parsed.extension.unwrap().manifest_path, path);
    }

    #[test]
    fn rejects_every_legacy_directive_shape() {
        for content in [
            "module github.com/acme/app\nvo ^1.0.0\n",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"1.0.0\"\nrequire github.com/acme/lib ^1.0.0\n",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"1.0.0\"\nreplace = {}\n",
            "schema = 1\nformat = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"1.0.0\"\n",
        ] {
            assert!(ModFile::parse(content).is_err(), "{content:?}");
        }
    }

    #[test]
    fn rejects_self_dependency_and_incompatible_major() {
        let content = r#"
format = 1
module = "github.com/acme/app"
version = "0.1.0"
vo = "1.0.0"
[dependencies]
"github.com/acme/app" = "^1.0.0"
"#;
        let error = ModFile::parse(content).unwrap_err().to_string();
        assert!(error.contains("must not depend on itself"), "{error}");

        let content = r#"
format = 1
module = "github.com/acme/app"
version = "0.1.0"
vo = "1.0.0"
[dependencies]
"github.com/acme/library/v2" = "^1.4.0"
"#;
        let error = ModFile::parse(content).unwrap_err().to_string();
        assert!(error.contains("incompatible with module path"), "{error}");
    }

    #[test]
    fn canonical_render_round_trips_every_section() {
        let content = r#"
format = 1
module = "github.com/acme/app"
version = "0.1.0"
vo = "1.0.0"

[dependencies]
"github.com/vo-lang/voplay" = "~0.7.2"
"github.com/acme/http" = "1.3.1"

[web]
entry = "src/main.vo"

[extension]
name = "demo"

[extension.native]
targets = ["aarch64-apple-darwin"]

[extension.wasm]
kind = "standalone"
wasm = "demo.wasm"

[extension.web]
provider_role = "ui-renderer"
provider_roles = ["ui-renderer"]
capabilities = ["clipboard-read"]

[extension.web.js]
renderer = "js/renderer'quoted.js"

[build.native]
kind = "cargo"
manifest = "rust/ext/Cargo.toml"
package = "demo-native"
"#;
        let mf = ModFile::parse(content).unwrap();
        let rendered = mf.render().unwrap();
        let reparsed = ModFile::parse(&rendered).unwrap();
        assert_eq!(reparsed, mf);
        assert_eq!(reparsed.render().unwrap(), rendered);
        let http = rendered.find("github.com/acme/http").unwrap();
        let voplay = rendered.find("github.com/vo-lang/voplay").unwrap();
        assert!(http < voplay);
        let native = rendered.find("[extension.native]").unwrap();
        let wasm = rendered.find("[extension.wasm]").unwrap();
        let web = rendered.find("[extension.web]").unwrap();
        assert!(native < wasm && wasm < web);
        assert!(rendered.contains("manifest = \"rust/ext/Cargo.toml\""));
    }

    #[test]
    fn render_rejects_invalid_public_values() {
        let mut file = ModFile::parse(
            r#"
format = 1
module = "github.com/acme/app"
version = "0.1.0"
vo = "1.0.0"
[dependencies]
"github.com/acme/lib" = "^1.0.0"
"#,
        )
        .unwrap();
        file.dependencies.push(file.dependencies[0].clone());
        assert!(file.render().is_err());
    }

    #[test]
    fn requires_root_identity_and_toolchain_and_bare_exact_versions() {
        assert!(ModFile::parse("vo = \"1.0.0\"\n").is_err());
        assert!(ModFile::parse(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n"
        )
        .is_err());
        let old_exact = r#"
format = 1
module = "github.com/acme/app"
version = "0.1.0"
vo = "1.0.0"
[dependencies]
"github.com/acme/lib" = "v1.2.3"
"#;
        assert!(ModFile::parse(old_exact).is_err());
    }

    #[test]
    fn web_entry_is_a_portable_vo_source_path() {
        for entry in ["../main.vo", "/main.vo", "src\\main.vo", "src/main.js"] {
            let content = format!(
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"1.0.0\"\n[web]\nentry = {entry:?}\n"
            );
            assert!(ModFile::parse(&content).is_err(), "accepted {entry:?}");
        }
        assert!(ModFile::parse(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"1.0.0\"\n[web]\nentry = \"cmd/web/main.vo\"\n"
        )
        .is_ok());
    }

    #[test]
    fn removed_publish_table_fails_closed() {
        let content = concat!(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
            "vo = \"1.0.0\"\n",
            "[publish]\n",
            "include = [\"assets\"]\n",
        );
        let error = ModFile::parse(content).unwrap_err().to_string();
        assert!(error.contains("unknown key"), "{error}");
        assert!(error.contains("publish"), "{error}");
    }
}
