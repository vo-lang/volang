use std::path::{Path, PathBuf};

use crate::cache::layout;
use crate::ext_manifest::{DeclaredArtifactId, ExtensionManifest};
use crate::identity::ArtifactId;
use crate::schema::lockfile::LockedModule;
use crate::schema::manifest::ManifestArtifact;
use crate::{Error, Result};

#[derive(Debug, Clone)]
pub struct RequiredArtifact<'a> {
    pub artifact: &'a ManifestArtifact,
    pub cache_relative_path: PathBuf,
}

pub fn required_artifacts_for_target<'a>(
    locked: &LockedModule,
    published_artifacts: &'a [ManifestArtifact],
    ext_manifest: Option<&ExtensionManifest>,
    target: &str,
) -> Result<Vec<RequiredArtifact<'a>>> {
    let Some(ext_manifest) = ext_manifest else {
        return Ok(Vec::new());
    };
    ext_manifest.validate()?;

    declared_artifacts_for_target(ext_manifest, target)
        .into_iter()
        .map(|declared| {
            let artifact = find_published_artifact(locked, published_artifacts, &declared)?;
            let cache_relative_path =
                artifact_relative_path(&artifact.id).map_err(Error::InvalidReleaseMetadata)?;
            Ok(RequiredArtifact {
                artifact,
                cache_relative_path,
            })
        })
        .collect()
}

pub fn find_locked_module_for_cache_dir<'a>(
    cache_root: &Path,
    module_dir: &Path,
    locked_modules: &'a [LockedModule],
) -> Option<&'a LockedModule> {
    let (module_path, version) = layout::module_identity_from_cache_dir(cache_root, module_dir)?;
    locked_modules
        .iter()
        .find(|locked| locked.path == module_path && locked.version == version)
}

fn declared_artifacts_for_target(
    ext_manifest: &ExtensionManifest,
    target: &str,
) -> Vec<DeclaredArtifactId> {
    ext_manifest
        .declared_artifact_ids()
        .into_iter()
        .filter(|artifact| artifact.target == target)
        .collect()
}

fn find_published_artifact<'a>(
    locked: &LockedModule,
    published_artifacts: &'a [ManifestArtifact],
    declared: &DeclaredArtifactId,
) -> Result<&'a ManifestArtifact> {
    let mut matches = published_artifacts.iter().filter(|artifact| {
        artifact.id.kind == declared.kind
            && artifact.id.target == declared.target
            && artifact.id.name == declared.name
    });
    let artifact = matches
        .next()
        .ok_or_else(|| Error::MissingReleaseArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: format!(
                "vo.release.json does not declare an {} artifact for {}@{} ({})",
                declared.kind, locked.path, locked.version, declared.name,
            ),
        })?;
    if matches.next().is_some() {
        return Err(Error::InvalidReleaseMetadata(format!(
            "vo.release.json declares the {} artifact for {}@{} more than once ({})",
            declared.kind, locked.path, locked.version, declared.name,
        )));
    }
    Ok(artifact)
}

/// Canonical cache location for one locked artifact.
///
/// Kind and target are part of the path because artifact names are only
/// unique inside their full `(kind, target, name)` identity. Keeping each
/// identity in a separate directory also prevents target switches from
/// reusing or overwriting another target's bytes.
pub fn artifact_relative_path(id: &ArtifactId) -> std::result::Result<PathBuf, String> {
    id.validate()?;
    Ok(PathBuf::from("artifacts")
        .join(&id.kind)
        .join(&id.target)
        .join(&id.name))
}

/// Platform-independent slash-separated key for cache collision checks and
/// deterministic metadata. Local filesystem paths are built separately with
/// [`artifact_relative_path`].
pub fn artifact_cache_key(id: &ArtifactId) -> std::result::Result<String, String> {
    id.validate()?;
    Ok(format!("artifacts/{}/{}/{}", id.kind, id.target, id.name))
}

/// Canonical flat release-asset name derived from the complete identity.
pub fn artifact_release_asset_name(id: &ArtifactId) -> std::result::Result<String, String> {
    id.validate()?;
    let identity = format!(
        "vo-artifact-asset-v1\0{}\0{}\0{}",
        id.kind, id.target, id.name
    );
    let digest = crate::digest::Digest::from_sha256(identity.as_bytes());
    let hex = digest
        .as_str()
        .strip_prefix("sha256:")
        .expect("SHA-256 digests always use the canonical prefix");
    Ok(format!("vo-artifact-v1-{hex}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::ext_manifest::parse_ext_manifest_content;
    use crate::identity::{ArtifactId, ModulePath};
    use crate::schema::lockfile::LockOrigin;
    use crate::schema::manifest::ManifestArtifact;
    use crate::version::ExactVersion;

    fn published_artifact(kind: &str, target: &str, name: &str, bytes: &[u8]) -> ManifestArtifact {
        ManifestArtifact {
            id: ArtifactId {
                kind: kind.to_string(),
                target: target.to_string(),
                name: name.to_string(),
            },
            size: bytes.len() as u64,
            digest: Digest::from_sha256(bytes),
        }
    }

    fn locked_module() -> LockedModule {
        LockedModule {
            path: ModulePath::parse("github.com/acme/demo").unwrap(),
            version: ExactVersion::parse("1.2.3").unwrap(),
            origin: LockOrigin::Registry,
            release: Some(
                Digest::parse(
                    "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                )
                .unwrap(),
            ),
            intent: None,
        }
    }

    fn native_manifest(target: &str) -> ExtensionManifest {
        parse_ext_manifest_content(
            &format!(
                r#"
format = 1
module = "github.com/acme/demo"
version = "1.2.3"
vo = "0.1.0"

[extension]
name = "demo"

[extension.native]
targets = ["{target}"]
"#,
            ),
            Path::new("vo.mod"),
        )
        .unwrap()
    }

    fn bindgen_wasm_manifest(wasm: &str, js: &str) -> ExtensionManifest {
        parse_ext_manifest_content(
            &format!(
                r#"
format = 1
module = "github.com/acme/demo"
version = "0.1.0"
vo = "0.1.0"

[extension]
name = "demo"

[extension.wasm]
kind = "bindgen"
wasm = "{wasm}"
js = "{js}"
"#,
            ),
            Path::new("vo.mod"),
        )
        .unwrap()
    }

    #[test]
    fn required_artifacts_for_target_resolves_native_artifact() {
        let locked = locked_module();
        let published = vec![published_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            "libdemo.dylib",
            b"native",
        )];
        let manifest = native_manifest("aarch64-apple-darwin");

        let required = required_artifacts_for_target(
            &locked,
            &published,
            Some(&manifest),
            "aarch64-apple-darwin",
        )
        .unwrap();

        assert_eq!(required.len(), 1);
        assert_eq!(required[0].artifact.id.kind, "extension-native");
        assert_eq!(required[0].artifact.id.name, "libdemo.dylib");
        assert_eq!(
            required[0].cache_relative_path,
            Path::new("artifacts/extension-native/aarch64-apple-darwin/libdemo.dylib")
        );
    }

    #[test]
    fn artifact_cache_paths_isolate_kind_and_target_identity() {
        let apple = ArtifactId {
            kind: "extension-native".to_string(),
            target: "aarch64-apple-darwin".to_string(),
            name: "libdemo.dylib".to_string(),
        };
        let linux = ArtifactId {
            kind: "extension-native".to_string(),
            target: "aarch64-unknown-linux-gnu".to_string(),
            name: "libdemo.dylib".to_string(),
        };

        assert_ne!(
            artifact_relative_path(&apple).unwrap(),
            artifact_relative_path(&linux).unwrap()
        );
        assert_eq!(
            artifact_relative_path(&apple).unwrap(),
            Path::new("artifacts/extension-native/aarch64-apple-darwin/libdemo.dylib")
        );
        assert_eq!(
            artifact_cache_key(&apple).unwrap(),
            "artifacts/extension-native/aarch64-apple-darwin/libdemo.dylib"
        );
        assert_ne!(
            artifact_release_asset_name(&apple).unwrap(),
            artifact_release_asset_name(&linux).unwrap()
        );
        assert_eq!(
            artifact_release_asset_name(&apple).unwrap(),
            "vo-artifact-v1-ad06ca6415ff9368f06399b215f0b685bc157864fecc7d76b035527739c69966"
        );
    }

    #[test]
    fn required_artifacts_for_target_resolves_wasm_and_js_artifacts() {
        let locked = locked_module();
        let published = vec![
            published_artifact(
                "extension-wasm",
                "wasm32-unknown-unknown",
                "demo_bg.wasm",
                b"wasm",
            ),
            published_artifact(
                "extension-js-glue",
                "wasm32-unknown-unknown",
                "demo.js",
                b"js",
            ),
        ];
        let manifest = bindgen_wasm_manifest("demo_bg.wasm", "demo.js");

        let required = required_artifacts_for_target(
            &locked,
            &published,
            Some(&manifest),
            "wasm32-unknown-unknown",
        )
        .unwrap();

        assert_eq!(required.len(), 2);
        assert_eq!(required[0].artifact.id.kind, "extension-js-glue");
        assert_eq!(required[1].artifact.id.kind, "extension-wasm");
        assert_eq!(
            required[0].cache_relative_path,
            Path::new("artifacts/extension-js-glue/wasm32-unknown-unknown/demo.js")
        );
        assert_eq!(
            required[1].cache_relative_path,
            Path::new("artifacts/extension-wasm/wasm32-unknown-unknown/demo_bg.wasm")
        );
    }

    #[test]
    fn required_artifacts_for_target_fails_when_release_is_missing_declared_artifact() {
        let locked = locked_module();
        let manifest = native_manifest("aarch64-apple-darwin");

        let err =
            required_artifacts_for_target(&locked, &[], Some(&manifest), "aarch64-apple-darwin")
                .unwrap_err();

        assert!(matches!(err, Error::MissingReleaseArtifact { .. }));
        assert!(
            err.to_string()
                .contains("vo.release.json does not declare an extension-native artifact"),
            "{}",
            err
        );
    }

    #[test]
    fn required_artifacts_for_target_rejects_duplicate_release_entries() {
        let locked = locked_module();
        let manifest = native_manifest("aarch64-apple-darwin");
        let artifact = published_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            "libdemo.dylib",
            b"native",
        );

        let error = required_artifacts_for_target(
            &locked,
            &[artifact.clone(), artifact],
            Some(&manifest),
            "aarch64-apple-darwin",
        )
        .unwrap_err();

        assert!(error.to_string().contains("more than once"), "{error}");
    }

    #[test]
    fn required_artifacts_for_target_revalidates_public_manifest_values() {
        let locked = locked_module();
        let published = vec![published_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            "libdemo.dylib",
            b"native",
        )];
        let mut manifest = native_manifest("aarch64-apple-darwin");
        manifest
            .native
            .as_mut()
            .unwrap()
            .targets
            .push("aarch64-apple-darwin".to_string());

        let error = required_artifacts_for_target(
            &locked,
            &published,
            Some(&manifest),
            "aarch64-apple-darwin",
        )
        .unwrap_err();

        assert!(error.to_string().contains("duplicate target"));
    }

    #[test]
    fn required_artifacts_for_target_returns_empty_for_unsupported_target() {
        let locked = locked_module();
        let published = vec![published_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            "libdemo.dylib",
            b"native",
        )];
        let manifest = native_manifest("aarch64-apple-darwin");

        let required = required_artifacts_for_target(
            &locked,
            &published,
            Some(&manifest),
            "x86_64-unknown-linux-gnu",
        )
        .unwrap();

        assert!(required.is_empty());
    }

    #[test]
    fn required_artifacts_for_target_returns_empty_without_manifest() {
        let locked = locked_module();
        let published = vec![published_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            "libdemo.dylib",
            b"native",
        )];

        let required =
            required_artifacts_for_target(&locked, &published, None, "aarch64-apple-darwin")
                .unwrap();

        assert!(required.is_empty());
    }

    #[test]
    fn find_locked_module_for_cache_dir_matches_locked_module() {
        let locked = locked_module();
        let cache_root = Path::new("cache");
        let module_dir =
            cache_root.join(layout::relative_module_dir(&locked.path, &locked.version));

        let found = find_locked_module_for_cache_dir(
            cache_root,
            &module_dir,
            std::slice::from_ref(&locked),
        )
        .unwrap();

        assert_eq!(found.path, locked.path);
        assert_eq!(found.version, locked.version);
    }
}
