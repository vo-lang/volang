use super::*;
use std::io::Cursor;
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

use crate::digest::Digest;
use crate::identity::ArtifactId;
use crate::identity::ModulePath;
use crate::schema::lockfile::LockedArtifact;
use crate::schema::manifest::{ManifestSource, ReleaseManifest};
use crate::version::ExactVersion;
use crate::version::ToolchainConstraint;

struct CountingRegistry {
    source_fetches: AtomicUsize,
    source_bytes: Vec<u8>,
}

impl CountingRegistry {
    fn new(source_bytes: Vec<u8>) -> Self {
        Self {
            source_fetches: AtomicUsize::new(0),
            source_bytes,
        }
    }
}

impl Registry for CountingRegistry {
    fn list_versions(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        Ok(Vec::new())
    }

    fn fetch_manifest(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
    ) -> Result<crate::schema::manifest::ReleaseManifest, Error> {
        Err(Error::RegistryError("unused in test".to_string()))
    }

    fn fetch_source_package(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        self.source_fetches.fetch_add(1, AtomicOrdering::Relaxed);
        Ok(self.source_bytes.clone())
    }

    fn fetch_artifact(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        Err(Error::RegistryError("unused in test".to_string()))
    }
}

fn test_locked_module(module: &str, version: &str, source_raw: &[u8]) -> (LockedModule, Vec<u8>) {
    let manifest = ReleaseManifest {
        schema_version: 1,
        module: ModulePath::parse(module).unwrap(),
        version: ExactVersion::parse(version).unwrap(),
        commit: "0123456789abcdef0123456789abcdef01234567".to_string(),
        module_root: ModulePath::parse(module).unwrap().module_root().to_string(),
        vo: ToolchainConstraint::parse("0.1.0").unwrap(),
        require: Vec::new(),
        source: ManifestSource {
            name: format!("{}-source.tar.gz", version),
            size: source_raw.len() as u64,
            digest: Digest::from_sha256(source_raw),
        },
        artifacts: Vec::new(),
    };
    let manifest_raw = format!("{}\n", manifest.render()).into_bytes();
    let locked = crate::lock::locked_module_from_manifest_raw(&manifest, &manifest_raw);
    (locked, manifest_raw)
}

fn write_cached_source(module_dir: &Path, locked: &LockedModule, manifest_raw: &[u8]) {
    std::fs::create_dir_all(module_dir).unwrap();
    std::fs::write(
        module_dir.join("vo.mod"),
        format!("module {}\nvo {}\n", locked.path, locked.vo),
    )
    .unwrap();
    std::fs::write(
        module_dir.join(VERSION_MARKER),
        format!("{}\n", locked.version),
    )
    .unwrap();
    std::fs::write(
        module_dir.join(SOURCE_DIGEST_MARKER),
        format!("{}\n", locked.source),
    )
    .unwrap();
    std::fs::write(module_dir.join("vo.release.json"), manifest_raw).unwrap();
}

fn build_source_archive(root: &str, files: &[(&str, &str)]) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    for (relative_path, content) in files {
        let full_path = format!("{root}/{relative_path}");
        let bytes = content.as_bytes();
        let mut header = tar::Header::new_gnu();
        header.set_size(bytes.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        builder
            .append_data(&mut header, full_path, Cursor::new(bytes))
            .unwrap();
    }
    builder.into_inner().unwrap().finish().unwrap()
}

#[test]
fn test_extract_source_entries_keeps_declared_include_files() {
    let archive = build_source_archive(
        "demo-v1.0.0",
        &[
            (
                "vo.mod",
                concat!(
                    "module github.com/acme/demo\n",
                    "vo 0.1.0\n\n",
                    "[extension]\n",
                    "name = \"demo\"\n",
                    "include = [\"js/dist/studio_renderer.js\"]\n\n",
                    "[extension.native]\n",
                    "path = \"rust/target/{profile}/libdemo\"\n\n",
                    "[[extension.native.targets]]\n",
                    "target = \"aarch64-apple-darwin\"\n",
                    "library = \"libdemo.dylib\"\n",
                ),
            ),
            ("main.vo", "package main\nfunc main() {}\n"),
            ("js/dist/studio_renderer.js", "export const renderer = 1;\n"),
            ("js/dist/ignored.js", "export const ignored = 1;\n"),
        ],
    );

    let entries = extract_source_entries(&archive).unwrap();
    let paths = entries
        .into_iter()
        .map(|(path, _)| path)
        .collect::<Vec<_>>();

    assert!(paths.contains(&PathBuf::from("vo.mod")));
    assert!(paths.contains(&PathBuf::from("main.vo")));
    assert!(paths.contains(&PathBuf::from("js/dist/studio_renderer.js")));
    assert!(!paths.contains(&PathBuf::from("js/dist/ignored.js")));
}

#[test]
fn test_extract_source_entries_keeps_declared_include_directories() {
    let archive = build_source_archive(
        "demo-v1.0.0",
        &[
            (
                "vo.mod",
                concat!(
                    "module github.com/acme/demo\n",
                    "vo 0.1.0\n\n",
                    "[extension]\n",
                    "name = \"demo\"\n",
                    "include = [\"js/dist\"]\n\n",
                    "[extension.native]\n",
                    "path = \"rust/target/{profile}/libdemo\"\n\n",
                    "[[extension.native.targets]]\n",
                    "target = \"aarch64-apple-darwin\"\n",
                    "library = \"libdemo.dylib\"\n",
                ),
            ),
            ("main.vo", "package main\nfunc main() {}\n"),
            (
                "js/dist/voplay-render-island.js",
                "export { bootstrapWebView } from './bootstrap_webview.js';\n",
            ),
            (
                "js/dist/bootstrap_webview.js",
                "export const bootstrapWebView = 1;\n",
            ),
            ("js/dist/nested/helper.js", "export const helper = 1;\n"),
        ],
    );

    let entries = extract_source_entries(&archive).unwrap();
    let paths = entries
        .into_iter()
        .map(|(path, _)| path)
        .collect::<Vec<_>>();

    assert!(paths.contains(&PathBuf::from("vo.mod")));
    assert!(paths.contains(&PathBuf::from("main.vo")));
    assert!(paths.contains(&PathBuf::from("js/dist/voplay-render-island.js")));
    assert!(paths.contains(&PathBuf::from("js/dist/bootstrap_webview.js")));
    assert!(paths.contains(&PathBuf::from("js/dist/nested/helper.js")));
}

struct ExactInstallRegistry {
    manifest: ReleaseManifest,
    source_bytes: Vec<u8>,
    source_fetches: AtomicUsize,
}

impl Registry for ExactInstallRegistry {
    fn list_versions(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        Ok(Vec::new())
    }

    fn fetch_manifest(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
    ) -> Result<ReleaseManifest, Error> {
        Ok(self.manifest.clone())
    }

    fn fetch_source_package(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        self.source_fetches.fetch_add(1, AtomicOrdering::Relaxed);
        Ok(self.source_bytes.clone())
    }

    fn fetch_artifact(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        Err(Error::RegistryError("unused in test".to_string()))
    }
}

#[test]
fn test_create_unique_stage_dir_returns_distinct_paths() {
    let temp = tempfile::tempdir().unwrap();
    let first = create_unique_stage_dir(temp.path(), "stage").unwrap();
    let second = create_unique_stage_dir(temp.path(), "stage").unwrap();

    assert_ne!(first.path(), second.path());
    assert!(first.path().is_dir());
    assert!(second.path().is_dir());
}

#[test]
fn test_atomic_write_bytes_replaces_existing_file() {
    let temp = tempfile::tempdir().unwrap();
    let path = temp.path().join("artifact.bin");

    std::fs::write(&path, b"old-bytes").unwrap();
    atomic_write_bytes(&path, b"new-bytes").unwrap();

    assert_eq!(std::fs::read(&path).unwrap(), b"new-bytes");
}

#[test]
fn test_download_source_skips_fetch_when_cache_is_already_valid() {
    let temp = tempfile::tempdir().unwrap();
    let source_raw = b"source-package";
    let (locked, manifest_raw) = test_locked_module("github.com/acme/lib", "v1.0.0", source_raw);
    let module_dir = crate::cache::layout::cache_dir(temp.path(), &locked.path, &locked.version);
    write_cached_source(&module_dir, &locked, &manifest_raw);
    let registry = CountingRegistry::new(b"unused-source".to_vec());

    download_source(
        temp.path(),
        &locked,
        &registry,
        "source.tar.gz",
        &manifest_raw,
    )
    .unwrap();

    assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 0);
}

#[test]
fn test_download_artifact_replaces_invalid_existing_cache_file() {
    let temp = tempfile::tempdir().unwrap();
    let source_raw = b"source-package";
    let (mut locked, manifest_raw) =
        test_locked_module("github.com/acme/lib", "v1.0.0", source_raw);
    let module_dir = crate::cache::layout::cache_dir(temp.path(), &locked.path, &locked.version);
    write_cached_source(&module_dir, &locked, &manifest_raw);

    let artifact_bytes = b"fresh-artifact";
    let artifact = LockedArtifact {
        id: ArtifactId {
            kind: "extension-native".to_string(),
            target: "aarch64-apple-darwin".to_string(),
            name: "libdemo.dylib".to_string(),
        },
        size: artifact_bytes.len() as u64,
        digest: Digest::from_sha256(artifact_bytes),
    };
    locked.artifacts.push(artifact.clone());

    let art_path = module_dir.join("artifacts").join(&artifact.id.name);
    std::fs::create_dir_all(art_path.parent().unwrap()).unwrap();
    std::fs::write(&art_path, b"stale").unwrap();

    struct ArtifactRegistry {
        bytes: Vec<u8>,
    }

    impl Registry for ArtifactRegistry {
        fn list_versions(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
            Ok(Vec::new())
        }

        fn fetch_manifest(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
        ) -> Result<crate::schema::manifest::ReleaseManifest, Error> {
            Err(Error::RegistryError("unused in test".to_string()))
        }

        fn fetch_source_package(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            Err(Error::RegistryError("unused in test".to_string()))
        }

        fn fetch_artifact(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
            _asset_name: &str,
        ) -> Result<Vec<u8>, Error> {
            Ok(self.bytes.clone())
        }
    }

    let registry = ArtifactRegistry {
        bytes: artifact_bytes.to_vec(),
    };
    download_artifact(temp.path(), &locked, &artifact, &registry).unwrap();

    assert_eq!(std::fs::read(&art_path).unwrap(), artifact_bytes);
}

#[test]
fn test_validate_source_cache_entry_preserves_locked_module_mismatch() {
    let temp = tempfile::tempdir().unwrap();
    let source_raw = b"source-package";
    let (mut locked, manifest_raw) =
        test_locked_module("github.com/acme/lib", "v1.0.0", source_raw);
    let module_dir = crate::cache::layout::cache_dir(temp.path(), &locked.path, &locked.version);
    write_cached_source(&module_dir, &locked, &manifest_raw);

    locked.artifacts.push(LockedArtifact {
        id: ArtifactId {
            kind: "extension-native".to_string(),
            target: "aarch64-apple-darwin".to_string(),
            name: "libdemo.dylib".to_string(),
        },
        size: 5,
        digest: Digest::parse(
            "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
        )
        .unwrap(),
    });

    let error = validate_source_cache_entry(temp.path(), &locked).unwrap_err();

    assert!(matches!(
        error,
        Error::LockedModuleMismatch { ref field, .. } if field == "artifacts"
    ));
}

#[test]
fn test_install_exact_module_rejects_packaged_ext_contract_mismatch() {
    let temp = tempfile::tempdir().unwrap();
    let source_bytes = build_source_archive(
        "lib-v1.2.3",
        &[
            (
                "vo.mod",
                concat!(
                    "module github.com/acme/lib\n",
                    "vo ^0.1.0\n\n",
                    "[extension]\n",
                    "name = \"lib\"\n\n",
                    "[extension.wasm]\n",
                    "type = \"bindgen\"\n",
                    "wasm = \"lib.wasm\"\n",
                    "js_glue = \"lib.js\"\n",
                ),
            ),
            ("lib.vo", "package lib\nfunc Hello() {}\n"),
        ],
    );
    let source_digest = Digest::from_sha256(&source_bytes);
    let manifest = ReleaseManifest::parse(&format!(
        r#"{{
  "schema_version": 1,
  "module": "github.com/acme/lib",
  "version": "v1.2.3",
  "commit": "0123456789abcdef0123456789abcdef01234567",
  "module_root": ".",
  "vo": "^0.1.0",
  "require": [],
  "source": {{
    "name": "lib-v1.2.3-source.tar.gz",
    "size": {},
    "digest": "{}"
  }},
  "artifacts": [
    {{
      "kind": "extension-wasm",
      "target": "wasm32-unknown-unknown",
      "name": "lib.wasm",
      "size": 4,
      "digest": "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    }}
  ]
}}"#,
        source_bytes.len(),
        source_digest,
    ))
    .unwrap();
    let registry = ExactInstallRegistry {
        manifest,
        source_bytes,
        source_fetches: AtomicUsize::new(0),
    };
    let module = ModulePath::parse("github.com/acme/lib").unwrap();
    let version = ExactVersion::parse("v1.2.3").unwrap();

    let error =
        install_exact_module(temp.path(), &registry, &module, &version, "vo test").unwrap_err();

    assert!(matches!(error, Error::InvalidReleaseMetadata(_)));
}

#[test]
fn test_install_exact_module_populates_cache_and_returns_locked_metadata() {
    let temp = tempfile::tempdir().unwrap();
    let source_bytes = build_source_archive(
        "lib-v1.2.3",
        &[
            ("vo.mod", "module github.com/acme/lib\nvo ^0.1.0\n"),
            ("lib.vo", "package lib\nfunc Hello() {}\n"),
        ],
    );
    let source_digest = Digest::from_sha256(&source_bytes);
    let manifest = ReleaseManifest::parse(&format!(
        r#"{{
  "schema_version": 1,
  "module": "github.com/acme/lib",
  "version": "v1.2.3",
  "commit": "0123456789abcdef0123456789abcdef01234567",
  "module_root": ".",
  "vo": "^0.1.0",
  "require": [],
  "source": {{
    "name": "lib-v1.2.3-source.tar.gz",
    "size": {},
    "digest": "{}"
  }},
  "artifacts": []
}}"#,
        source_bytes.len(),
        source_digest
    ))
    .unwrap();
    let registry = ExactInstallRegistry {
        manifest: manifest.clone(),
        source_bytes: source_bytes.clone(),
        source_fetches: AtomicUsize::new(0),
    };
    let module = ModulePath::parse("github.com/acme/lib").unwrap();
    let version = ExactVersion::parse("v1.2.3").unwrap();

    let installed =
        install_exact_module(temp.path(), &registry, &module, &version, "vo test").unwrap();

    assert_eq!(
        installed.cache_dir,
        crate::cache::layout::cache_dir(temp.path(), &module, &version)
    );
    assert_eq!(installed.locked.path, module);
    assert_eq!(installed.locked.version, version);
    assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 1);
    assert_eq!(
        std::fs::read_to_string(installed.cache_dir.join("vo.mod")).unwrap(),
        "module github.com/acme/lib\nvo ^0.1.0\n"
    );
    assert_eq!(
        std::fs::read_to_string(installed.cache_dir.join(VERSION_MARKER))
            .unwrap()
            .trim(),
        "v1.2.3"
    );
    assert_eq!(
        std::fs::read_to_string(installed.cache_dir.join(SOURCE_DIGEST_MARKER))
            .unwrap()
            .trim(),
        source_digest.to_string()
    );
}
