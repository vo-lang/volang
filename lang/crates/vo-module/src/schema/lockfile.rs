use serde::{Deserialize, Serialize};

use crate::digest::Digest;
use crate::identity::ModulePath;
use crate::profile::{ArtifactRole, CapabilitySet};
use crate::version::ExactVersion;
use crate::Error;

/// Current canonical `vo.lock` format.
pub const LOCK_FILE_VERSION: u64 = 1;

/// The one exact dependency selection for a project.
///
/// Edges and toolchain requirements live in the intent descriptors bound by
/// each node. Copying them into the lock would create a second authority.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockFile {
    pub format: u64,
    /// Typed digest of the root `vo.mod` intent.
    pub root: Digest,
    #[serde(default, rename = "module")]
    pub modules: Vec<LockedModule>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LockOrigin {
    Registry,
    Workspace,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockedModule {
    pub path: ModulePath,
    pub version: ExactVersion,
    pub origin: LockOrigin,
    /// Digest of the exact `vo.release.json` bytes for registry nodes.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub release: Option<Digest>,
    /// Typed `vo.mod` intent digest for workspace nodes.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub intent: Option<Digest>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub selection: Option<LockedCapabilitySelection>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum LockedArtifactMode {
    Published,
    SourceRecipe,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockedRoleArtifact {
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
pub struct LockedCapabilitySelection {
    #[serde(default)]
    pub requested_by: Vec<String>,
    pub capabilities: Vec<String>,
    pub target: String,
    pub toolchain: String,
    pub schema: Digest,
    pub abi: Digest,
    pub vo_graph: Digest,
    pub rust_graph: Digest,
    pub js_graph: Digest,
    pub recipe_graph: Digest,
    pub mode: LockedArtifactMode,
    #[serde(default, rename = "role_artifact")]
    pub role_artifacts: Vec<LockedRoleArtifact>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_recipe: Option<Digest>,
    #[serde(default, rename = "source_output")]
    pub source_outputs: Vec<crate::profile::SourceRoleOutputDeclaration>,
}

impl LockFile {
    pub fn parse(content: &str) -> Result<Self, Error> {
        if content.len() > crate::MAX_LOCK_FILE_BYTES {
            return Err(Error::LockFileParse(format!(
                "vo.lock exceeds the {}-byte lock-file limit",
                crate::MAX_LOCK_FILE_BYTES
            )));
        }
        let lock: Self = toml::from_str(content)
            .map_err(|error| Error::LockFileParse(format!("TOML parse error: {error}")))?;
        lock.validate()?;
        Ok(lock)
    }

    pub fn validate(&self) -> Result<(), Error> {
        if self.format != LOCK_FILE_VERSION {
            return Err(Error::LockFileParse(format!(
                "unsupported lock file format: {}",
                self.format
            )));
        }
        if self.modules.is_empty() {
            return Err(Error::LockFileParse(
                "vo.lock must contain at least one [[module]] entry; dependency-free roots omit vo.lock"
                    .to_string(),
            ));
        }
        validate_locked_module_graph(&self.modules)
    }

    /// Render deterministic TOML. Readers deliberately accept equivalent TOML.
    pub fn render(&self) -> Result<String, Error> {
        self.validate()?;
        let mut output = super::BoundedTextOutput::new(crate::MAX_LOCK_FILE_BYTES)
            .map_err(Error::LockFileParse)?;
        macro_rules! push {
            ($value:expr) => {
                output.push_str($value).map_err(Error::LockFileParse)?
            };
        }
        macro_rules! quoted {
            ($value:expr) => {
                output
                    .push_toml_string($value)
                    .map_err(Error::LockFileParse)?
            };
        }

        push!("format = 1\nroot = ");
        quoted!(self.root.as_str());
        push!("\n");

        let mut modules = self.modules.iter().collect::<Vec<_>>();
        modules.sort_by(|left, right| left.path.cmp(&right.path));
        for module in modules {
            push!("\n[[module]]\npath = ");
            quoted!(module.path.as_str());
            push!("\nversion = ");
            quoted!(&module.version.to_string());
            push!("\norigin = \"");
            push!(match module.origin {
                LockOrigin::Registry => "registry",
                LockOrigin::Workspace => "workspace",
            });
            push!("\"\n");
            match module.origin {
                LockOrigin::Registry => {
                    push!("release = ");
                    quoted!(module
                        .release
                        .as_ref()
                        .expect("validated registry release")
                        .as_str());
                    push!("\n");
                }
                LockOrigin::Workspace => {
                    push!("intent = ");
                    quoted!(module
                        .intent
                        .as_ref()
                        .expect("validated workspace intent")
                        .as_str());
                    push!("\n");
                }
            }
            if let Some(selection) = &module.selection {
                push!("\n[module.selection]\nrequested_by = [");
                append_string_array(&mut output, &selection.requested_by)?;
                push!("]\ncapabilities = [");
                append_string_array(&mut output, &selection.capabilities)?;
                push!("]\ntarget = ");
                quoted!(&selection.target);
                push!("\ntoolchain = ");
                quoted!(&selection.toolchain);
                for (name, digest) in [
                    ("schema", &selection.schema),
                    ("abi", &selection.abi),
                    ("vo_graph", &selection.vo_graph),
                    ("rust_graph", &selection.rust_graph),
                    ("js_graph", &selection.js_graph),
                    ("recipe_graph", &selection.recipe_graph),
                ] {
                    push!("\n");
                    push!(name);
                    push!(" = ");
                    quoted!(digest.as_str());
                }
                push!("\nmode = \"");
                push!(match selection.mode {
                    LockedArtifactMode::Published => "published",
                    LockedArtifactMode::SourceRecipe => "source-recipe",
                });
                push!("\"\n");
                if let Some(source_recipe) = &selection.source_recipe {
                    push!("source_recipe = ");
                    quoted!(source_recipe.as_str());
                    push!("\n");
                }
                for output in &selection.source_outputs {
                    push!("\n[[module.selection.source_output]]\nrole = ");
                    quoted!(output.role.as_str());
                    push!("\nkind = ");
                    quoted!(&output.kind);
                    push!("\nname = ");
                    quoted!(&output.name);
                    push!("\n");
                }
                for artifact in &selection.role_artifacts {
                    push!("\n[[module.selection.role_artifact]]\nrole = ");
                    quoted!(artifact.role.as_str());
                    push!("\nkind = ");
                    quoted!(&artifact.kind);
                    push!("\nname = ");
                    quoted!(&artifact.name);
                    push!("\ndigest = ");
                    quoted!(artifact.digest.as_str());
                    push!("\nsbom = ");
                    quoted!(artifact.sbom.as_str());
                    push!("\ncapability_manifest = ");
                    quoted!(artifact.capability_manifest.as_str());
                    push!("\nprovenance = ");
                    quoted!(artifact.provenance.as_str());
                    push!("\n");
                }
            }
        }
        Ok(output.finish())
    }

    pub fn find(&self, path: &ModulePath) -> Option<&LockedModule> {
        self.modules.iter().find(|module| module.path == *path)
    }
}

pub(crate) fn validate_locked_module_graph(modules: &[LockedModule]) -> Result<(), Error> {
    if modules.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::LockFileParse(format!(
            "module contains more than {} entries",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }
    for pair in modules.windows(2) {
        if pair[0].path >= pair[1].path {
            return Err(Error::LockFileParse(
                "[[module]] entries must be unique and sorted by path".to_string(),
            ));
        }
    }
    for (index, module) in modules.iter().enumerate() {
        if !module.path.accepts_version(&module.version) {
            return Err(Error::LockFileParse(format!(
                "module[{index}].version {} is incompatible with module path {}",
                module.version, module.path
            )));
        }
        match module.origin {
            LockOrigin::Registry if module.path.is_local() => {
                return Err(Error::LockFileParse(format!(
                    "module[{index}] local ModuleId requires workspace origin"
                )));
            }
            LockOrigin::Registry if module.release.is_some() && module.intent.is_none() => {}
            LockOrigin::Workspace if module.intent.is_some() && module.release.is_none() => {}
            LockOrigin::Registry => {
                return Err(Error::LockFileParse(format!(
                    "module[{index}] registry origin requires only 'release'"
                )));
            }
            LockOrigin::Workspace => {
                return Err(Error::LockFileParse(format!(
                    "module[{index}] workspace origin requires only 'intent'"
                )));
            }
        }
        if let Some(selection) = &module.selection {
            validate_selection(selection, index)?;
        }
    }
    Ok(())
}

fn validate_selection(
    selection: &LockedCapabilitySelection,
    module_index: usize,
) -> Result<(), Error> {
    let normalized = CapabilitySet::normalize(
        &selection.capabilities,
        &format!("module[{module_index}].selection.capabilities"),
    )
    .map_err(|error| Error::LockFileParse(error.to_string()))?;
    if normalized.is_empty() || normalized.as_slice() != selection.capabilities {
        return Err(Error::LockFileParse(format!(
            "module[{module_index}].selection.capabilities must be non-empty, sorted, and unique"
        )));
    }
    if selection.requested_by.is_empty() {
        return Err(Error::LockFileParse(format!(
            "module[{module_index}].selection.requested_by must contain at least one source"
        )));
    }
    for pair in selection.requested_by.windows(2) {
        if pair[0] >= pair[1] {
            return Err(Error::LockFileParse(format!(
                "module[{module_index}].selection.requested_by must be sorted and unique"
            )));
        }
    }
    if selection.target.is_empty() || selection.toolchain.is_empty() {
        return Err(Error::LockFileParse(format!(
            "module[{module_index}].selection target and toolchain must be non-empty"
        )));
    }
    let mut role_ids = std::collections::BTreeSet::new();
    for artifact in &selection.role_artifacts {
        crate::schema::validate_file_name(&artifact.name)
            .map_err(|detail| Error::LockFileParse(format!("module[{module_index}]: {detail}")))?;
        if !role_ids.insert((
            &artifact.role,
            artifact.kind.as_str(),
            artifact.name.as_str(),
        )) {
            return Err(Error::LockFileParse(format!(
                "module[{module_index}].selection contains a duplicate role artifact"
            )));
        }
    }
    let mut source_output_ids = std::collections::BTreeSet::new();
    for output in &selection.source_outputs {
        crate::schema::validate_file_name(&output.kind)
            .map_err(|detail| Error::LockFileParse(format!("module[{module_index}]: {detail}")))?;
        crate::schema::validate_file_name(&output.name)
            .map_err(|detail| Error::LockFileParse(format!("module[{module_index}]: {detail}")))?;
        if !source_output_ids.insert((&output.role, output.kind.as_str(), output.name.as_str())) {
            return Err(Error::LockFileParse(format!(
                "module[{module_index}].selection contains a duplicate source output"
            )));
        }
    }
    match selection.mode {
        LockedArtifactMode::Published
            if selection.source_recipe.is_none()
                && selection.source_outputs.is_empty()
                && !selection.role_artifacts.is_empty() => {}
        LockedArtifactMode::SourceRecipe
            if selection.source_recipe.is_some()
                && selection.role_artifacts.is_empty()
                && !selection.source_outputs.is_empty() => {}
        LockedArtifactMode::Published => {
            return Err(Error::LockFileParse(format!(
                "module[{module_index}] published selection requires role artifacts and no source recipe"
            )));
        }
        LockedArtifactMode::SourceRecipe => {
            return Err(Error::LockFileParse(format!(
                "module[{module_index}] source-recipe selection requires only source_recipe"
            )));
        }
    }
    Ok(())
}

fn append_string_array(
    output: &mut super::BoundedTextOutput,
    items: &[String],
) -> Result<(), Error> {
    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            output.push_str(", ").map_err(Error::LockFileParse)?;
        }
        output
            .push_toml_string(item)
            .map_err(Error::LockFileParse)?;
    }
    Ok(())
}

pub(crate) fn validate_materialized_module_limits(modules: &[LockedModule]) -> Result<(), Error> {
    if modules.len() > crate::MAX_MODULE_DEPENDENCIES {
        return Err(Error::LockFileParse(format!(
            "materialized subset contains more than {} modules",
            crate::MAX_MODULE_DEPENDENCIES
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const GOLDEN: &str = r#"format = 1
root = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

[[module]]
path = "example.com/acme/render"
version = "0.4.2"
origin = "registry"
release = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

[[module]]
path = "example.com/acme/ui"
version = "0.7.3"
origin = "workspace"
intent = "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
"#;

    #[test]
    fn parses_and_roundtrips_lock() {
        let lock = LockFile::parse(GOLDEN).unwrap();
        assert_eq!(lock.format, LOCK_FILE_VERSION);
        assert_eq!(lock.modules.len(), 2);
        assert_eq!(lock.render().unwrap(), GOLDEN);
    }

    #[test]
    fn accepts_equivalent_toml_and_rejects_ambiguous_origins() {
        assert!(LockFile::parse(&GOLDEN.replace("format = 1", "format=1")).is_ok());
        assert!(LockFile::parse(&GOLDEN.replace(
            "release = \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"",
            "intent = \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\""
        ))
        .is_err());
    }
}
