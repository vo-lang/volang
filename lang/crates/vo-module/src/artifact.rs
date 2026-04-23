use std::path::{Path, PathBuf};

use crate::cache::layout;
use crate::ext_manifest::{DeclaredArtifactId, ExtensionManifest};
use crate::schema::lockfile::{LockedArtifact, LockedModule};
use crate::{Error, Result};

#[derive(Debug, Clone)]
pub struct RequiredArtifact<'a> {
    pub locked_artifact: &'a LockedArtifact,
    pub cache_relative_path: PathBuf,
}

pub fn required_artifacts_for_target<'a>(
    locked: &'a LockedModule,
    ext_manifest: Option<&ExtensionManifest>,
    target: &str,
) -> Result<Vec<RequiredArtifact<'a>>> {
    let Some(ext_manifest) = ext_manifest else {
        return Ok(Vec::new());
    };

    declared_artifacts_for_target(ext_manifest, target)
        .into_iter()
        .map(|declared| {
            let locked_artifact = find_locked_artifact(locked, &declared)?;
            Ok(RequiredArtifact {
                locked_artifact,
                cache_relative_path: artifact_relative_path(locked_artifact),
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

fn find_locked_artifact<'a>(
    locked: &'a LockedModule,
    declared: &DeclaredArtifactId,
) -> Result<&'a LockedArtifact> {
    locked
        .artifacts
        .iter()
        .find(|artifact| {
            artifact.id.kind == declared.kind
                && artifact.id.target == declared.target
                && artifact.id.name == declared.name
        })
        .ok_or_else(|| Error::MissingLockedArtifact {
            module: locked.path.as_str().to_string(),
            version: locked.version.to_string(),
            detail: format!(
                "vo.lock does not pin an {} artifact for {}@{} ({})",
                declared.kind, locked.path, locked.version, declared.name,
            ),
        })
}

fn artifact_relative_path(artifact: &LockedArtifact) -> PathBuf {
    PathBuf::from("artifacts").join(&artifact.id.name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::ext_manifest::parse_ext_manifest_content;
    use crate::identity::{ArtifactId, ModulePath};
    use crate::version::{ExactVersion, ToolchainConstraint};

    fn locked_artifact(kind: &str, target: &str, name: &str, bytes: &[u8]) -> LockedArtifact {
        LockedArtifact {
            id: ArtifactId {
                kind: kind.to_string(),
                target: target.to_string(),
                name: name.to_string(),
            },
            size: bytes.len() as u64,
            digest: Digest::from_sha256(bytes),
        }
    }

    fn locked_module(artifacts: Vec<LockedArtifact>) -> LockedModule {
        LockedModule {
            path: ModulePath::parse("github.com/acme/demo").unwrap(),
            version: ExactVersion::parse("v1.2.3").unwrap(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            commit: "1111111111111111111111111111111111111111".to_string(),
            release_manifest: Digest::parse(
                "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            )
            .unwrap(),
            source: Digest::parse(
                "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            )
            .unwrap(),
            deps: Vec::new(),
            artifacts,
        }
    }

    fn native_manifest(target: &str, library: &str) -> ExtensionManifest {
        parse_ext_manifest_content(
            &format!(
                r#"
[extension]
name = "demo"

[extension.native]
path = "rust/target/{{profile}}/libdemo"

[[extension.native.targets]]
target = "{target}"
library = "{library}"
"#,
            ),
            Path::new("vo.mod"),
        )
        .unwrap()
    }

    fn bindgen_wasm_manifest(wasm: &str, js_glue: &str) -> ExtensionManifest {
        parse_ext_manifest_content(
            &format!(
                r#"
[extension]
name = "demo"

[extension.wasm]
type = "bindgen"
wasm = "{wasm}"
js_glue = "{js_glue}"
"#,
            ),
            Path::new("vo.mod"),
        )
        .unwrap()
    }

    #[test]
    fn required_artifacts_for_target_resolves_native_artifact() {
        let locked = locked_module(vec![locked_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            "libdemo.dylib",
            b"native",
        )]);
        let manifest = native_manifest("aarch64-apple-darwin", "libdemo.dylib");

        let required =
            required_artifacts_for_target(&locked, Some(&manifest), "aarch64-apple-darwin")
                .unwrap();

        assert_eq!(required.len(), 1);
        assert_eq!(required[0].locked_artifact.id.kind, "extension-native");
        assert_eq!(required[0].locked_artifact.id.name, "libdemo.dylib");
        assert_eq!(
            required[0].cache_relative_path,
            Path::new("artifacts/libdemo.dylib")
        );
    }

    #[test]
    fn required_artifacts_for_target_resolves_wasm_and_js_glue() {
        let locked = locked_module(vec![
            locked_artifact(
                "extension-wasm",
                "wasm32-unknown-unknown",
                "demo_bg.wasm",
                b"wasm",
            ),
            locked_artifact(
                "extension-js-glue",
                "wasm32-unknown-unknown",
                "demo.js",
                b"js",
            ),
        ]);
        let manifest = bindgen_wasm_manifest("demo_bg.wasm", "demo.js");

        let required =
            required_artifacts_for_target(&locked, Some(&manifest), "wasm32-unknown-unknown")
                .unwrap();

        assert_eq!(required.len(), 2);
        assert_eq!(required[0].locked_artifact.id.kind, "extension-js-glue");
        assert_eq!(required[1].locked_artifact.id.kind, "extension-wasm");
        assert_eq!(
            required[0].cache_relative_path,
            Path::new("artifacts/demo.js")
        );
        assert_eq!(
            required[1].cache_relative_path,
            Path::new("artifacts/demo_bg.wasm")
        );
    }

    #[test]
    fn required_artifacts_for_target_fails_when_lock_is_missing_declared_artifact() {
        let locked = locked_module(Vec::new());
        let manifest = native_manifest("aarch64-apple-darwin", "libdemo.dylib");

        let err = required_artifacts_for_target(&locked, Some(&manifest), "aarch64-apple-darwin")
            .unwrap_err();

        assert!(matches!(err, Error::MissingLockedArtifact { .. }));
        assert!(
            err.to_string()
                .contains("vo.lock does not pin an extension-native artifact"),
            "{}",
            err
        );
    }

    #[test]
    fn required_artifacts_for_target_returns_empty_for_unsupported_target() {
        let locked = locked_module(vec![locked_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            "libdemo.dylib",
            b"native",
        )]);
        let manifest = native_manifest("aarch64-apple-darwin", "libdemo.dylib");

        let required =
            required_artifacts_for_target(&locked, Some(&manifest), "x86_64-unknown-linux-gnu")
                .unwrap();

        assert!(required.is_empty());
    }

    #[test]
    fn required_artifacts_for_target_returns_empty_without_manifest() {
        let locked = locked_module(vec![locked_artifact(
            "extension-native",
            "aarch64-apple-darwin",
            "libdemo.dylib",
            b"native",
        )]);

        let required =
            required_artifacts_for_target(&locked, None, "aarch64-apple-darwin").unwrap();

        assert!(required.is_empty());
    }

    #[test]
    fn find_locked_module_for_cache_dir_matches_locked_module() {
        let locked = locked_module(Vec::new());
        let cache_root = Path::new("cache");
        let module_dir = cache_root.join(layout::relative_module_dir(
            locked.path.as_str(),
            &locked.version,
        ));

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
