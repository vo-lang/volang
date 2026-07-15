use super::*;
use std::io::Cursor;
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

use crate::digest::Digest;
use crate::identity::ArtifactId;
use crate::identity::ModulePath;
use crate::schema::lockfile::LockedArtifact;
use crate::schema::manifest::{ManifestSource, ManifestWebManifest, ReleaseManifest};
use crate::schema::{canonical_source_file_set, SourceFileEntry};
use crate::version::ExactVersion;
use crate::version::ToolchainConstraint;

fn initialize_cache_root(root: &Path) {
    drop(crate::cache::acquire_read_lease(root).unwrap());
}

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
    fn list_version_candidates(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        Ok(Vec::new())
    }

    fn fetch_manifest_raw(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
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
        _artifact: &crate::identity::ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        Err(Error::RegistryError("unused in test".to_string()))
    }
}

struct ArtifactBytesRegistry {
    bytes: Vec<u8>,
}

impl Registry for ArtifactBytesRegistry {
    fn list_version_candidates(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        Ok(Vec::new())
    }

    fn fetch_manifest_raw(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
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
        _artifact: &crate::identity::ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        Ok(self.bytes.clone())
    }
}

fn test_locked_module(module: &str, version: &str, source_raw: &[u8]) -> (LockedModule, Vec<u8>) {
    let mod_content = format!("module {module}\nvo 0.1.0\n");
    test_locked_module_with_contract(module, version, source_raw, &mod_content, &[])
}

fn test_locked_module_with_contract(
    module: &str,
    version: &str,
    source_raw: &[u8],
    mod_content: &str,
    artifacts: &[LockedArtifact],
) -> (LockedModule, Vec<u8>) {
    let source_set = canonical_source_file_set(&[SourceFileEntry {
        path: "vo.mod".to_string(),
        size: mod_content.len() as u64,
        digest: Digest::from_sha256(mod_content.as_bytes()),
    }])
    .unwrap();
    let web_manifest = test_web_manifest(
        mod_content,
        version,
        &[SourceFileEntry {
            path: "vo.mod".to_string(),
            size: mod_content.len() as u64,
            digest: Digest::from_sha256(mod_content.as_bytes()),
        }],
        artifacts,
    );
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
            files_size: source_set.total_size,
            files_digest: source_set.digest,
        },
        web_manifest: ManifestWebManifest {
            size: web_manifest.len() as u64,
            digest: Digest::from_sha256(web_manifest.as_bytes()),
        },
        artifacts: artifacts
            .iter()
            .map(|artifact| crate::schema::manifest::ManifestArtifact {
                id: artifact.id.clone(),
                size: artifact.size,
                digest: artifact.digest.clone(),
            })
            .collect(),
    };
    let manifest_raw = format!("{}\n", manifest.render().unwrap()).into_bytes();
    let locked = crate::lock::locked_module_from_manifest_raw(&manifest, &manifest_raw);
    (locked, manifest_raw)
}

fn write_cached_source(module_dir: &Path, locked: &LockedModule, manifest_raw: &[u8]) {
    let mod_content = format!("module {}\nvo {}\n", locked.path, locked.vo);
    write_cached_source_with_contract(module_dir, locked, manifest_raw, &mod_content, &[]);
}

fn write_cached_source_with_contract(
    module_dir: &Path,
    locked: &LockedModule,
    manifest_raw: &[u8],
    mod_content: &str,
    artifacts: &[LockedArtifact],
) {
    std::fs::create_dir_all(module_dir).unwrap();
    std::fs::write(module_dir.join("vo.mod"), mod_content).unwrap();
    let source_entries = [SourceFileEntry {
        path: "vo.mod".to_string(),
        size: mod_content.len() as u64,
        digest: Digest::from_sha256(mod_content.as_bytes()),
    }];
    std::fs::write(
        module_dir.join("vo.web.json"),
        test_web_manifest(
            mod_content,
            &locked.version.to_string(),
            &source_entries,
            artifacts,
        ),
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
    if !files
        .iter()
        .any(|(relative_path, _)| *relative_path == "vo.web.json")
    {
        let full_path = format!("{root}/vo.web.json");
        let generated = files
            .iter()
            .find(|(path, _)| *path == "vo.mod")
            .map(|(_, mod_content)| {
                let (entries, _, _) = test_source_contract(files);
                test_web_manifest(mod_content, "v1.0.0", &entries, &[])
            })
            .unwrap_or_else(|| "{}\n".to_string());
        let bytes = generated.as_bytes();
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

fn build_source_archive_bytes(root: &str, files: &[(&str, &[u8])]) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    for (relative_path, bytes) in files {
        let full_path = format!("{root}/{relative_path}");
        let mut header = tar::Header::new_gnu();
        header.set_size(bytes.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        builder
            .append_data(&mut header, full_path, Cursor::new(*bytes))
            .unwrap();
    }
    builder.into_inner().unwrap().finish().unwrap()
}

fn test_source_contract(files: &[(&str, &str)]) -> (Vec<SourceFileEntry>, u64, Digest) {
    let mut entries = files
        .iter()
        .filter(|(path, _)| crate::schema::is_source_file_set_candidate(path).unwrap())
        .map(|(path, content)| SourceFileEntry {
            path: (*path).to_string(),
            size: content.len() as u64,
            digest: Digest::from_sha256(content.as_bytes()),
        })
        .collect::<Vec<_>>();
    entries.sort_by(|left, right| left.path.cmp(&right.path));
    let set = canonical_source_file_set(&entries).unwrap();
    (entries, set.total_size, set.digest)
}

fn test_web_manifest(
    mod_content: &str,
    version: &str,
    source_entries: &[SourceFileEntry],
    artifacts: &[LockedArtifact],
) -> String {
    let mod_file = crate::schema::modfile::ModFile::parse(mod_content).unwrap();
    let module = mod_file.module.as_github().unwrap();
    let source_set = canonical_source_file_set(source_entries).unwrap();
    let require = mod_file
        .require
        .iter()
        .map(|requirement| {
            serde_json::json!({
                "module": requirement.module.as_str(),
                "constraint": requirement.constraint.to_string(),
            })
        })
        .collect::<Vec<_>>();
    let extension = mod_file.extension.as_ref().map(|extension| {
        serde_json::json!({
            "name": extension.name,
            "include": extension
                .include
                .iter()
                .map(|path| path.to_string_lossy().replace('\\', "/"))
                .collect::<Vec<_>>(),
            "wasm": extension.wasm,
            "web": extension.web,
        })
    });
    let artifacts = artifacts
        .iter()
        .filter(|artifact| artifact.id.kind != "extension-native")
        .map(|artifact| {
            serde_json::json!({
                "kind": artifact.id.kind,
                "target": artifact.id.target,
                "name": artifact.id.name,
                "path": artifact.id.name,
                "size": artifact.size,
                "digest": artifact.digest,
            })
        })
        .collect::<Vec<_>>();
    format!(
        "{}\n",
        serde_json::to_string_pretty(&serde_json::json!({
            "schema_version": 1,
            "module": module.as_str(),
            "version": version,
            "commit": "0123456789abcdef0123456789abcdef01234567",
            "module_root": module.module_root(),
            "vo": mod_file.vo.to_string(),
            "require": require,
            "source_digest": source_set.digest,
            "source": source_entries,
            "web": mod_file.web,
            "extension": extension,
            "artifacts": artifacts,
        }))
        .unwrap()
    )
}

fn build_source_archive_with_raw_path(raw_path: &[u8]) -> Vec<u8> {
    assert!(raw_path.len() < 100);
    let content = b"package source\n";
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    let mut header = tar::Header::new_gnu();
    header.as_mut_bytes()[..raw_path.len()].copy_from_slice(raw_path);
    header.set_size(content.len() as u64);
    header.set_mode(0o644);
    header.set_entry_type(tar::EntryType::Regular);
    header.set_cksum();
    builder.append(&header, Cursor::new(content)).unwrap();
    builder.into_inner().unwrap().finish().unwrap()
}

#[test]
fn source_archives_reject_noncanonical_wire_paths_before_host_conversion() {
    for raw_path in [
        b"demo-v1.0.0/src\\main.vo".as_slice(),
        b"demo-v1.0.0//main.vo".as_slice(),
        b"C:/main.vo".as_slice(),
        "demo-e\u{301}/main.vo".as_bytes(),
        b"demo-v1.0.0./main.vo".as_slice(),
    ] {
        let error =
            extract_source_entries(&build_source_archive_with_raw_path(raw_path)).unwrap_err();
        assert!(
            error.contains("not portable") || error.contains("root component"),
            "{}: {error}",
            String::from_utf8_lossy(raw_path)
        );
    }

    let error = extract_source_entries(&build_source_archive_with_raw_path(
        b"demo-v1.0.0/main-\xff.vo",
    ))
    .unwrap_err();
    assert!(error.contains("valid UTF-8"), "{error}");
}

#[test]
fn source_archives_reject_portable_path_collisions() {
    let archive = build_source_archive(
        "demo-v1.0.0",
        &[
            ("Source/main.vo", "package source\n"),
            ("source/other.vo", "package source\n"),
        ],
    );
    let error = extract_source_entries(&archive).unwrap_err();
    assert!(
        error.contains("conflicts with portable spelling"),
        "{error}"
    );

    let archive = build_source_archive(
        "demo-v1.0.0",
        &[("assets", "file"), ("assets/icon.svg", "icon")],
    );
    let error = extract_source_entries(&archive).unwrap_err();
    assert!(error.contains("descends through file"), "{error}");

    for reserved in [
        "VO.RELEASE.JSON",
        "vo.releaſe.json",
        "artifactſ/demo.wasm",
        ".vo-ſource-digest",
    ] {
        let archive = build_source_archive(
            "demo-v1.0.0",
            &[
                ("vo.mod", "module github.com/acme/demo\nvo ^0.1.0\n"),
                (reserved, "reserved"),
            ],
        );
        let error = extract_source_entries(&archive).unwrap_err();
        assert!(
            error.contains("reserved for module-cache metadata"),
            "{reserved}: {error}"
        );
    }
}

#[test]
fn test_extract_source_entries_materializes_the_exact_web_source_set() {
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
    assert!(paths.contains(&PathBuf::from("js/dist/ignored.js")));
}

#[test]
fn test_extract_source_entries_rejects_unlisted_utf8_and_ignores_unlisted_binary_payloads() {
    let mod_content = "module github.com/acme/demo\nvo 0.1.0\n";
    let source_entries = [SourceFileEntry {
        path: "vo.mod".to_string(),
        size: mod_content.len() as u64,
        digest: Digest::from_sha256(mod_content.as_bytes()),
    }];
    let web = test_web_manifest(mod_content, "v1.0.0", &source_entries, &[]);
    let unexpected_utf8 = build_source_archive_bytes(
        "demo-v1.0.0",
        &[
            ("vo.mod", mod_content.as_bytes()),
            ("vo.web.json", web.as_bytes()),
            ("README.md", b"unlisted documentation"),
        ],
    );
    let error = extract_source_entries(&unexpected_utf8).unwrap_err();
    assert!(error.contains("README.md"), "{error}");
    assert!(error.contains("not declared"), "{error}");

    let binary_payload = build_source_archive_bytes(
        "demo-v1.0.0",
        &[
            ("vo.mod", mod_content.as_bytes()),
            ("vo.web.json", web.as_bytes()),
            ("release-payload.bin", &[0xff, 0xfe, 0xfd]),
        ],
    );
    let entries = extract_source_entries(&binary_payload).unwrap();
    assert_eq!(
        entries
            .iter()
            .map(|(path, _)| path.as_path())
            .collect::<Vec<_>>(),
        vec![Path::new("vo.mod"), Path::new("vo.web.json")],
    );
}

#[test]
fn source_packages_use_the_shared_portable_path_budget() {
    let mod_content = "module github.com/acme/demo\nvo 0.1.0\n";
    let maximum_component = "a".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES);
    let source_entries = [
        SourceFileEntry {
            path: maximum_component.clone(),
            size: 0,
            digest: Digest::from_sha256(b""),
        },
        SourceFileEntry {
            path: "vo.mod".to_string(),
            size: mod_content.len() as u64,
            digest: Digest::from_sha256(mod_content.as_bytes()),
        },
    ];
    let web = test_web_manifest(mod_content, "v1.0.0", &source_entries, &[]);
    let archive = build_source_archive_bytes(
        "demo-v1.0.0",
        &[
            ("vo.mod", mod_content.as_bytes()),
            (&maximum_component, b""),
            ("vo.web.json", web.as_bytes()),
        ],
    );
    let extracted = extract_source_entries(&archive).unwrap();
    assert!(extracted
        .iter()
        .any(|(path, _)| path == Path::new(&maximum_component)));

    let oversized_component = "a".repeat(
        crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES
            .checked_add(1)
            .unwrap(),
    );
    let archive = build_source_archive_bytes(
        "demo-v1.0.0",
        &[
            ("vo.mod", mod_content.as_bytes()),
            (&oversized_component, b"x"),
            ("vo.web.json", web.as_bytes()),
        ],
    );
    let error = extract_source_entries(&archive).unwrap_err();
    assert!(error.contains("not portable"), "{error}");
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

fn pure_source_registry() -> (ExactInstallRegistry, ModulePath, ExactVersion, Digest) {
    let mod_content = "module github.com/acme/lib\nvo ^0.1.0\n";
    let lib_content = "package lib\nfunc Hello() {}\n";
    let source_files = [("vo.mod", mod_content), ("lib.vo", lib_content)];
    let (source_entries, files_size, files_digest) = test_source_contract(&source_files);
    let web_manifest = test_web_manifest(mod_content, "v1.2.3", &source_entries, &[]);
    let source_bytes = build_source_archive(
        "lib-v1.2.3",
        &[
            ("vo.mod", mod_content),
            ("lib.vo", lib_content),
            ("vo.web.json", &web_manifest),
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
    "digest": "{}",
    "files_size": {},
    "files_digest": "{}"
  }},
  "web_manifest": {{
    "size": {},
    "digest": "{}"
  }},
  "artifacts": []
}}"#,
        source_bytes.len(),
        source_digest,
        files_size,
        files_digest,
        web_manifest.len(),
        Digest::from_sha256(web_manifest.as_bytes()),
    ))
    .unwrap();
    (
        ExactInstallRegistry {
            manifest,
            source_bytes,
            source_fetches: AtomicUsize::new(0),
        },
        ModulePath::parse("github.com/acme/lib").unwrap(),
        ExactVersion::parse("v1.2.3").unwrap(),
        source_digest,
    )
}

impl Registry for ExactInstallRegistry {
    fn list_version_candidates(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        Ok(Vec::new())
    }

    fn fetch_manifest_raw(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        Ok(format!("{}\n", self.manifest.render()?).into_bytes())
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
        _artifact: &crate::identity::ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        Err(Error::RegistryError("unused in test".to_string()))
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn cache_transactions_use_distinct_anchored_stage_directories() {
    let temp = tempfile::tempdir().unwrap();
    let cache_lock = crate::cache::mutation_lock::CacheMutationLock::shared(temp.path()).unwrap();
    let first = cache_lock.begin_transaction("stage").unwrap();
    let second = cache_lock.begin_transaction("stage").unwrap();

    assert_ne!(first.relative_path(), second.relative_path());
    assert!(temp.path().join(first.relative_path()).is_dir());
    assert!(temp.path().join(second.relative_path()).is_dir());
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn transaction_names_are_fixed_length_and_files_are_created_exclusively() {
    let temp = tempfile::tempdir().unwrap();
    let maximum_artifact_name = "a".repeat(255);
    let cache_lock = crate::cache::mutation_lock::CacheMutationLock::shared(temp.path()).unwrap();
    let staged = cache_lock
        .begin_transaction(&maximum_artifact_name)
        .unwrap();
    staged
        .write_file(Path::new("payload"), b"new-bytes")
        .unwrap();

    assert!(staged.relative_path().file_name().unwrap().len() < 128);
    assert_eq!(
        staged.read_file(Path::new("payload"), 9).unwrap(),
        b"new-bytes",
    );
}

#[test]
fn test_download_source_skips_fetch_when_cache_is_already_valid() {
    let temp = tempfile::tempdir().unwrap();
    initialize_cache_root(temp.path());
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
        source_raw.len() as u64,
        &manifest_raw,
    )
    .unwrap();

    assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 0);
}

#[test]
fn test_download_artifact_preserves_invalid_existing_cache_file() {
    let temp = tempfile::tempdir().unwrap();
    initialize_cache_root(temp.path());
    let source_raw = b"source-package";
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
    let mod_content = concat!(
        "module github.com/acme/lib\n",
        "vo 0.1.0\n\n",
        "[extension]\n",
        "name = \"lib\"\n\n",
        "[extension.native]\n",
        "path = \"rust/target/{profile}/libdemo\"\n\n",
        "[[extension.native.targets]]\n",
        "target = \"aarch64-apple-darwin\"\n",
        "library = \"libdemo.dylib\"\n",
    );
    let (locked, manifest_raw) = test_locked_module_with_contract(
        "github.com/acme/lib",
        "v1.0.0",
        source_raw,
        mod_content,
        std::slice::from_ref(&artifact),
    );
    let module_dir = crate::cache::layout::cache_dir(temp.path(), &locked.path, &locked.version);
    write_cached_source_with_contract(
        &module_dir,
        &locked,
        &manifest_raw,
        mod_content,
        std::slice::from_ref(&artifact),
    );

    let art_path = module_dir.join(crate::artifact::artifact_relative_path(&artifact.id).unwrap());
    std::fs::create_dir_all(art_path.parent().unwrap()).unwrap();
    std::fs::write(&art_path, b"stale").unwrap();

    struct ArtifactRegistry {
        bytes: Vec<u8>,
    }

    impl Registry for ArtifactRegistry {
        fn list_version_candidates(
            &self,
            _module: &ModulePath,
        ) -> Result<Vec<ExactVersion>, Error> {
            Ok(Vec::new())
        }

        fn fetch_manifest_raw(
            &self,
            _module: &ModulePath,
            _version: &ExactVersion,
        ) -> Result<Vec<u8>, Error> {
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
            _artifact: &crate::identity::ArtifactId,
        ) -> Result<Vec<u8>, Error> {
            Ok(self.bytes.clone())
        }
    }

    let registry = ArtifactRegistry {
        bytes: artifact_bytes.to_vec(),
    };
    let error = download_artifact(temp.path(), &locked, &artifact, &registry).unwrap_err();

    assert!(
        error.to_string().contains("clean the module cache"),
        "{error}"
    );
    assert_eq!(std::fs::read(&art_path).unwrap(), b"stale");
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn native_artifact_install_propagates_committed_durability_failure() {
    let temp = tempfile::tempdir().unwrap();
    initialize_cache_root(temp.path());
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
    let mod_content = concat!(
        "module github.com/acme/lib\n",
        "vo 0.1.0\n\n",
        "[extension]\n",
        "name = \"lib\"\n\n",
        "[extension.native]\n",
        "path = \"rust/target/{profile}/libdemo\"\n\n",
        "[[extension.native.targets]]\n",
        "target = \"aarch64-apple-darwin\"\n",
        "library = \"libdemo.dylib\"\n",
    );
    let (locked, manifest_raw) = test_locked_module_with_contract(
        "github.com/acme/lib",
        "v1.0.0",
        b"source-package",
        mod_content,
        std::slice::from_ref(&artifact),
    );
    let module_dir = crate::cache::layout::cache_dir(temp.path(), &locked.path, &locked.version);
    write_cached_source_with_contract(
        &module_dir,
        &locked,
        &manifest_raw,
        mod_content,
        std::slice::from_ref(&artifact),
    );
    let registry = ArtifactBytesRegistry {
        bytes: artifact_bytes.to_vec(),
    };
    let artifact_path =
        module_dir.join(crate::artifact::artifact_relative_path(&artifact.id).unwrap());
    crate::cache::mutation_lock::fail_publication_sync_for_test(&artifact_path);

    let error = download_artifact(temp.path(), &locked, &artifact, &registry).unwrap_err();

    assert!(matches!(
        error,
        Error::CachePublicationDurabilityUnconfirmed { ref path, .. }
            if path.ends_with("libdemo.dylib")
    ));
    let read_lock = crate::cache::mutation_lock::CacheMutationLock::shared(temp.path()).unwrap();
    validate_artifact_cache_entry_with_fs(&read_lock.file_system(), &locked, &artifact).unwrap();
    download_artifact(temp.path(), &locked, &artifact, &registry).unwrap();
}

#[cfg(unix)]
#[test]
fn native_artifact_install_rejects_symlinked_parent_without_touching_target() {
    use std::os::unix::fs::symlink;

    let root = tempfile::tempdir().unwrap();
    initialize_cache_root(root.path());
    let outside = tempfile::tempdir().unwrap();
    let sentinel = outside.path().join("keep.bin");
    std::fs::write(&sentinel, b"preserve").unwrap();
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
    let mod_content = concat!(
        "module github.com/acme/lib\n",
        "vo 0.1.0\n\n",
        "[extension]\n",
        "name = \"lib\"\n\n",
        "[extension.native]\n",
        "path = \"rust/target/{profile}/libdemo\"\n\n",
        "[[extension.native.targets]]\n",
        "target = \"aarch64-apple-darwin\"\n",
        "library = \"libdemo.dylib\"\n",
    );
    let (locked, manifest_raw) = test_locked_module_with_contract(
        "github.com/acme/lib",
        "v1.0.0",
        b"source-package",
        mod_content,
        std::slice::from_ref(&artifact),
    );
    let module_dir = crate::cache::layout::cache_dir(root.path(), &locked.path, &locked.version);
    write_cached_source_with_contract(
        &module_dir,
        &locked,
        &manifest_raw,
        mod_content,
        std::slice::from_ref(&artifact),
    );
    symlink(outside.path(), module_dir.join("artifacts")).unwrap();
    let registry = CountingRegistry::new(Vec::new());

    let error = download_artifact(root.path(), &locked, &artifact, &registry).unwrap_err();

    assert!(
        error
            .to_string()
            .contains("cache-owned artifacts path must be a directory"),
        "{error}"
    );
    assert_eq!(std::fs::read(&sentinel).unwrap(), b"preserve");
}

#[test]
fn test_validate_source_cache_entry_preserves_locked_module_mismatch() {
    let temp = tempfile::tempdir().unwrap();
    let source_raw = b"source-package";
    let (mut locked, manifest_raw) =
        test_locked_module("github.com/acme/lib", "v1.0.0", source_raw);
    initialize_cache_root(temp.path());
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

    let read_lock = crate::cache::mutation_lock::CacheMutationLock::shared(temp.path()).unwrap();
    let error = validate_source_cache_entry_with_fs(&read_lock.file_system(), &locked).unwrap_err();

    assert!(matches!(
        error,
        Error::LockedModuleMismatch { ref field, .. } if field == "artifacts"
    ));
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn anchored_post_lease_validation_rejects_a_valid_replacement_cache() {
    let parent = tempfile::tempdir().unwrap();
    let root = parent.path().join("cache");
    let moved_root = parent.path().join("moved-cache");
    let replacement_root = parent.path().join("replacement-cache");
    std::fs::create_dir(&root).unwrap();
    std::fs::create_dir(&replacement_root).unwrap();
    initialize_cache_root(&root);
    initialize_cache_root(&replacement_root);

    let artifact_bytes = b"valid-artifact";
    let artifact = LockedArtifact {
        id: ArtifactId {
            kind: "extension-native".to_string(),
            target: "aarch64-apple-darwin".to_string(),
            name: "libdemo.dylib".to_string(),
        },
        size: artifact_bytes.len() as u64,
        digest: Digest::from_sha256(artifact_bytes),
    };
    let mod_content = concat!(
        "module github.com/acme/lib\n",
        "vo 0.1.0\n\n",
        "[extension]\n",
        "name = \"lib\"\n\n",
        "[extension.native]\n",
        "path = \"rust/target/{profile}/libdemo\"\n\n",
        "[[extension.native.targets]]\n",
        "target = \"aarch64-apple-darwin\"\n",
        "library = \"libdemo.dylib\"\n",
    );
    let (locked, manifest_raw) = test_locked_module_with_contract(
        "github.com/acme/lib",
        "v1.0.0",
        b"source-package",
        mod_content,
        std::slice::from_ref(&artifact),
    );
    for cache_root in [&root, &replacement_root] {
        let module_dir = crate::cache::layout::cache_dir(cache_root, &locked.path, &locked.version);
        write_cached_source_with_contract(
            &module_dir,
            &locked,
            &manifest_raw,
            mod_content,
            std::slice::from_ref(&artifact),
        );
        let artifact_path =
            module_dir.join(crate::artifact::artifact_relative_path(&artifact.id).unwrap());
        std::fs::create_dir_all(artifact_path.parent().unwrap()).unwrap();
        std::fs::write(artifact_path, artifact_bytes).unwrap();
    }

    let mutation_lock = crate::cache::mutation_lock::CacheMutationLock::shared(&root).unwrap();
    std::fs::rename(&root, &moved_root).unwrap();
    std::fs::rename(&replacement_root, &root).unwrap();

    let replacement_fs = vo_common::vfs::RealFs::new(&root);
    validate_source_cache_entry_with_fs(&replacement_fs, &locked).unwrap();
    validate_artifact_cache_entry_with_fs(&replacement_fs, &locked, &artifact).unwrap();
    let source_error =
        validate_source_cache_entry_with_fs(&mutation_lock.file_system(), &locked).unwrap_err();
    let artifact_error =
        validate_artifact_cache_entry_with_fs(&mutation_lock.file_system(), &locked, &artifact)
            .unwrap_err();

    assert!(source_error.to_string().contains("changed identity"));
    assert!(artifact_error.to_string().contains("changed identity"));
    assert!(!is_source_cached_anchored(&mutation_lock, &locked));
    assert!(!is_artifact_cached_anchored(
        &mutation_lock,
        &locked,
        &artifact,
    ));
}

#[test]
fn test_install_exact_module_rejects_packaged_ext_contract_mismatch() {
    let temp = tempfile::tempdir().unwrap();
    let mod_content = concat!(
        "module github.com/acme/lib\n",
        "vo ^0.1.0\n\n",
        "[extension]\n",
        "name = \"lib\"\n\n",
        "[extension.wasm]\n",
        "type = \"bindgen\"\n",
        "wasm = \"lib.wasm\"\n",
        "js_glue = \"lib.js\"\n",
    );
    let lib_content = "package lib\nfunc Hello() {}\n";
    let artifact = LockedArtifact {
        id: ArtifactId {
            kind: "extension-wasm".to_string(),
            target: "wasm32-unknown-unknown".to_string(),
            name: "lib.wasm".to_string(),
        },
        size: 4,
        digest: Digest::parse(
            "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
        )
        .unwrap(),
    };
    let source_files = [("vo.mod", mod_content), ("lib.vo", lib_content)];
    let (source_entries, files_size, files_digest) = test_source_contract(&source_files);
    let web_manifest = test_web_manifest(
        mod_content,
        "v1.2.3",
        &source_entries,
        std::slice::from_ref(&artifact),
    );
    let source_bytes = build_source_archive(
        "lib-v1.2.3",
        &[
            ("vo.mod", mod_content),
            ("lib.vo", lib_content),
            ("vo.web.json", &web_manifest),
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
    "digest": "{}",
    "files_size": {},
    "files_digest": "{}"
  }},
  "web_manifest": {{
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
        files_size,
        files_digest,
        web_manifest.len(),
        Digest::from_sha256(web_manifest.as_bytes()),
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
    let (registry, module, version, source_digest) = pure_source_registry();

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

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn native_source_install_propagates_committed_durability_failure() {
    let temp = tempfile::tempdir().unwrap();
    let (registry, module, version, _) = pure_source_registry();
    let destination = crate::cache::layout::cache_dir(temp.path(), &module, &version);
    crate::cache::mutation_lock::fail_publication_sync_for_test(&destination);

    let error =
        install_exact_module(temp.path(), &registry, &module, &version, "vo test").unwrap_err();

    assert!(matches!(
        error,
        Error::CachePublicationDurabilityUnconfirmed { ref path, .. }
            if path.ends_with("github.com@acme@lib/v1.2.3")
    ));
    let manifest_raw = format!("{}\n", registry.manifest.render().unwrap()).into_bytes();
    let locked = crate::lock::locked_module_from_manifest_raw(&registry.manifest, &manifest_raw);
    let read_lock = crate::cache::mutation_lock::CacheMutationLock::shared(temp.path()).unwrap();
    validate_source_cache_entry_with_fs(&read_lock.file_system(), &locked).unwrap();
    install_exact_module(temp.path(), &registry, &module, &version, "vo test").unwrap();
}

#[cfg(unix)]
#[test]
fn native_source_install_rejects_symlinked_cache_components_without_touching_targets() {
    use std::os::unix::fs::symlink;

    for symlink_version in [false, true] {
        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let outside = tempfile::tempdir().unwrap();
        let sentinel = outside.path().join("keep.vo");
        std::fs::write(&sentinel, b"package keep\n").unwrap();
        let (registry, module, version, _) = pure_source_registry();
        let cache_key = crate::cache::layout::cache_key(&module);
        if symlink_version {
            std::fs::create_dir(root.path().join(&cache_key)).unwrap();
            symlink(
                outside.path(),
                root.path().join(&cache_key).join(version.to_string()),
            )
            .unwrap();
        } else {
            symlink(outside.path(), root.path().join(&cache_key)).unwrap();
        }

        let error =
            install_exact_module(root.path(), &registry, &module, &version, "vo test").unwrap_err();

        assert!(
            error.to_string().contains("symbolic link") || error.to_string().contains("Symlink"),
            "{error}"
        );
        assert_eq!(std::fs::read(&sentinel).unwrap(), b"package keep\n");
    }
}
