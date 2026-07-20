use serde::{Deserialize, Serialize};

use crate::digest::Digest;
use crate::identity::ModulePath;
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
