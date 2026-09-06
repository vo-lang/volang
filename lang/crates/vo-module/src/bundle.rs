//! Deterministic source-only registry releases embedded by higher-level tools.

use std::io::Cursor;

use flate2::{Compression, GzBuilder};
use tar::{Builder, Header};

use crate::digest::Digest;
use crate::identity::{ArtifactId, ModulePath};
use crate::registry::Registry;
use crate::schema::manifest::{
    ManifestDependency, ManifestSource, ReleaseManifest, SOURCE_ARCHIVE_ASSET_NAME,
    SOURCE_ARCHIVE_TOP_LEVEL_DIR,
};
use crate::schema::modfile::ModFile;
use crate::schema::{SourceFileEntry, SourceFileMode, TreeManifest};
use crate::version::ExactVersion;
use crate::Error;

#[derive(Clone, Copy, Debug)]
pub struct BundledSourceFile<'a> {
    pub path: &'a str,
    pub bytes: &'a [u8],
    pub mode: SourceFileMode,
}

impl<'a> BundledSourceFile<'a> {
    pub const fn regular(path: &'a str, bytes: &'a [u8]) -> Self {
        Self {
            path,
            bytes,
            mode: SourceFileMode::Regular,
        }
    }
}

/// One immutable source-only registry release. Higher-level products can
/// embed their official modules while preserving the ordinary solver, lock,
/// digest, archive, and cache contracts.
#[derive(Clone, Debug)]
pub struct BundledSourceRegistry {
    manifest: ReleaseManifest,
    manifest_raw: Vec<u8>,
    source_package: Vec<u8>,
}

impl BundledSourceRegistry {
    pub fn new(files: &[BundledSourceFile<'_>]) -> Result<Self, Error> {
        let mut files = files.to_vec();
        files.sort_by(|left, right| left.path.as_bytes().cmp(right.path.as_bytes()));
        if files
            .windows(2)
            .any(|pair| pair[0].path.as_bytes() == pair[1].path.as_bytes())
        {
            return Err(Error::InvalidReleaseMetadata(
                "bundled source paths must be unique".to_string(),
            ));
        }
        let manifest_bytes = files
            .iter()
            .find(|file| file.path == "vo.mod")
            .ok_or_else(|| {
                Error::InvalidReleaseMetadata(
                    "bundled source release must contain vo.mod".to_string(),
                )
            })?
            .bytes;
        let manifest_text = std::str::from_utf8(manifest_bytes).map_err(|error| {
            Error::InvalidReleaseMetadata(format!("bundled source vo.mod must be UTF-8: {error}"))
        })?;
        let mod_file = ModFile::parse(manifest_text)?;
        let module = mod_file.module.as_public().cloned().ok_or_else(|| {
            Error::InvalidReleaseMetadata(
                "bundled source release requires a public module identity".to_string(),
            )
        })?;
        if mod_file.extension.is_some() {
            return Err(Error::InvalidReleaseMetadata(
                "bundled source-only releases cannot declare extension artifacts".to_string(),
            ));
        }

        let tree = TreeManifest {
            format: 1,
            files: files
                .iter()
                .map(|file| SourceFileEntry {
                    path: file.path.to_string(),
                    mode: file.mode,
                    size: u64::try_from(file.bytes.len()).unwrap_or(u64::MAX),
                    digest: Digest::from_sha256(file.bytes),
                })
                .collect(),
        };
        let tree_raw = tree.render()?;
        let source_package = write_source_package(&files, &tree_raw)?;
        if u64::try_from(source_package.len()).unwrap_or(u64::MAX) > crate::MAX_SOURCE_ARCHIVE_BYTES
        {
            return Err(Error::InvalidReleaseMetadata(format!(
                "bundled source package exceeds the {}-byte limit",
                crate::MAX_SOURCE_ARCHIVE_BYTES
            )));
        }

        let mut dependencies = mod_file
            .dependencies
            .iter()
            .map(|dependency| ManifestDependency {
                module: dependency.module.clone(),
                constraint: dependency.constraint.clone(),
                profile: dependency.capability_request.profile.clone(),
                capabilities: dependency
                    .capability_request
                    .capabilities
                    .as_slice()
                    .to_vec(),
            })
            .collect::<Vec<_>>();
        dependencies.sort_by(|left, right| left.module.cmp(&right.module));
        let manifest = ReleaseManifest {
            format: 1,
            module,
            version: mod_file.version.clone(),
            vo: mod_file.vo.clone(),
            intent: crate::lock::module_intent_digest(&mod_file)?,
            dependencies,
            profiles: mod_file.profiles.declarations(),
            default_profile: mod_file.profiles.default_profile().map(str::to_string),
            capabilities: mod_file.capabilities.declarations(),
            artifact_variants: Vec::new(),
            source_recipes: Vec::new(),
            source: ManifestSource {
                name: SOURCE_ARCHIVE_ASSET_NAME.to_string(),
                size: u64::try_from(source_package.len()).unwrap_or(u64::MAX),
                digest: Digest::from_sha256(&source_package),
                tree: Digest::from_sha256(&tree_raw),
            },
            artifacts: Vec::new(),
        };
        let manifest_raw = manifest.render()?.into_bytes();
        Ok(Self {
            manifest,
            manifest_raw,
            source_package,
        })
    }

    pub fn manifest(&self) -> &ReleaseManifest {
        &self.manifest
    }
}

impl Registry for BundledSourceRegistry {
    fn list_version_candidates(&self, module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        if module == &self.manifest.module {
            Ok(vec![self.manifest.version.clone()])
        } else {
            Err(not_found(module, None))
        }
    }

    fn fetch_manifest_raw(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        self.require_release(module, version)?;
        Ok(self.manifest_raw.clone())
    }

    fn fetch_source_package(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        self.require_release(module, version)?;
        if asset_name != SOURCE_ARCHIVE_ASSET_NAME {
            return Err(Error::RegistryNotFound {
                resource: format!("{module}@{version}/{asset_name}"),
            });
        }
        Ok(self.source_package.clone())
    }

    fn fetch_artifact(
        &self,
        module: &ModulePath,
        version: &ExactVersion,
        artifact: &ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        self.require_release(module, version)?;
        Err(Error::RegistryNotFound {
            resource: format!("{module}@{version}/{artifact:?}"),
        })
    }
}

impl BundledSourceRegistry {
    fn require_release(&self, module: &ModulePath, version: &ExactVersion) -> Result<(), Error> {
        if module == &self.manifest.module && version == &self.manifest.version {
            Ok(())
        } else {
            Err(not_found(module, Some(version)))
        }
    }
}

fn not_found(module: &ModulePath, version: Option<&ExactVersion>) -> Error {
    Error::RegistryNotFound {
        resource: version.map_or_else(
            || module.to_string(),
            |version| format!("{module}@{version}"),
        ),
    }
}

fn write_source_package(
    files: &[BundledSourceFile<'_>],
    tree_raw: &[u8],
) -> Result<Vec<u8>, Error> {
    let encoder = GzBuilder::new()
        .mtime(0)
        .write(Vec::new(), Compression::default());
    let mut builder = Builder::new(encoder);
    let mut tree_appended = false;
    for file in files {
        if !tree_appended && file.path.as_bytes() > b"vo.tree.json".as_slice() {
            append_source_file(
                &mut builder,
                "vo.tree.json",
                tree_raw,
                SourceFileMode::Regular,
            )?;
            tree_appended = true;
        }
        append_source_file(&mut builder, file.path, file.bytes, file.mode)?;
    }
    if !tree_appended {
        append_source_file(
            &mut builder,
            "vo.tree.json",
            tree_raw,
            SourceFileMode::Regular,
        )?;
    }
    let encoder = builder
        .into_inner()
        .map_err(|error| Error::InvalidReleaseMetadata(error.to_string()))?;
    encoder
        .finish()
        .map_err(|error| Error::InvalidReleaseMetadata(error.to_string()))
}

fn append_source_file(
    builder: &mut Builder<flate2::write::GzEncoder<Vec<u8>>>,
    path: &str,
    bytes: &[u8],
    mode: SourceFileMode,
) -> Result<(), Error> {
    let mut header = Header::new_gnu();
    header.set_size(u64::try_from(bytes.len()).unwrap_or(u64::MAX));
    header.set_mode(if mode.is_executable() { 0o755 } else { 0o644 });
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(0);
    header.set_cksum();
    builder
        .append_data(
            &mut header,
            format!("{SOURCE_ARCHIVE_TOP_LEVEL_DIR}/{path}"),
            Cursor::new(bytes),
        )
        .map_err(|error| Error::InvalidReleaseMetadata(error.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    const MOD: &[u8] =
        b"format = 1\nmodule = \"example.com/acme/bundled\"\nversion = \"0.1.4\"\nvo = \"0.1.0\"\n";

    #[test]
    fn bundled_source_release_is_deterministic_and_materializes_normally() {
        let files = [
            BundledSourceFile::regular("lib.vo", b"package bundled\n"),
            BundledSourceFile::regular("vo.mod", MOD),
        ];
        let first = BundledSourceRegistry::new(&files).unwrap();
        let second = BundledSourceRegistry::new(&files).unwrap();
        assert_eq!(first.manifest_raw, second.manifest_raw);
        assert_eq!(first.source_package, second.source_package);

        let project = crate::test_tempdir().unwrap();
        std::fs::write(
            project.path().join("vo.mod"),
            "format = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"example.com/acme/bundled\" = \"^0.1.4\"\n",
        )
        .unwrap();
        let cache = crate::test_tempdir().unwrap();
        crate::ops::mod_sync(project.path(), &first).unwrap();
        crate::ops::mod_fetch_with_options(
            project.path(),
            cache.path(),
            &first,
            &crate::project::ProjectContextOptions::new(
                crate::workspace::WorkspaceDiscovery::Disabled,
            ),
        )
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(
                crate::cache::layout::cache_dir(
                    cache.path(),
                    &ModulePath::parse("example.com/acme/bundled").unwrap(),
                    &ExactVersion::parse("0.1.4").unwrap(),
                )
                .join("lib.vo")
            )
            .unwrap(),
            "package bundled\n"
        );
    }
}
