use super::*;

use std::collections::BTreeMap;
use std::io::{Cursor, Read, Write};
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

use crate::digest::Digest;
use crate::identity::{ArtifactId, ModulePath};
use crate::schema::manifest::{ManifestArtifact, ManifestSource, ReleaseManifest};
use crate::schema::{SourceFileEntry, TreeManifest};
use crate::version::{ExactVersion, ToolchainConstraint};

type SourceFiles = Vec<(String, Vec<u8>)>;

fn initialize_cache_root(root: &Path) {
    drop(crate::cache::acquire_read_lease(root).unwrap());
}

fn source_entries(files: &SourceFiles) -> Vec<SourceFileEntry> {
    let mut entries = files
        .iter()
        .map(|(path, bytes)| SourceFileEntry {
            path: path.clone(),
            mode: crate::schema::SourceFileMode::Regular,
            size: u64::try_from(bytes.len()).unwrap(),
            digest: Digest::from_sha256(bytes),
        })
        .collect::<Vec<_>>();
    entries.sort_by(|left, right| left.path.cmp(&right.path));
    crate::schema::validate_package_file_set(&entries).unwrap();
    entries
}

fn tree_bytes(files: &SourceFiles) -> Vec<u8> {
    TreeManifest {
        format: 1,
        files: source_entries(files),
    }
    .render()
    .unwrap()
}

fn append_archive_file(
    builder: &mut tar::Builder<flate2::write::GzEncoder<Vec<u8>>>,
    path: &str,
    bytes: &[u8],
) {
    append_archive_file_with_mode(builder, path, bytes, 0o644);
}

fn append_archive_file_with_mode(
    builder: &mut tar::Builder<flate2::write::GzEncoder<Vec<u8>>>,
    path: &str,
    bytes: &[u8],
    mode: u32,
) {
    let mut header = tar::Header::new_gnu();
    header.set_size(u64::try_from(bytes.len()).unwrap());
    header.set_mode(mode);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(0);
    header.set_cksum();
    builder
        .append_data(&mut header, path, Cursor::new(bytes))
        .unwrap();
}

fn archive_with_modes(entries: &[(&str, &[u8], u32)]) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    for (path, bytes, mode) in entries {
        append_archive_file_with_mode(&mut builder, path, bytes, *mode);
    }
    builder.into_inner().unwrap().finish().unwrap()
}

fn archive_with_package(root: &str, files: &SourceFiles, tree_raw: &[u8]) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    let mut entries = files
        .iter()
        .map(|(path, bytes)| (format!("{root}/{path}"), bytes.as_slice()))
        .collect::<Vec<_>>();
    entries.push((format!("{root}/vo.tree.json"), tree_raw));
    entries.sort_by(|left, right| left.0.cmp(&right.0));
    for (path, bytes) in entries {
        append_archive_file(&mut builder, &path, bytes);
    }
    builder.into_inner().unwrap().finish().unwrap()
}

fn raw_archive(entries: &[(&str, &[u8])]) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    for (path, bytes) in entries {
        append_archive_file(&mut builder, path, bytes);
    }
    builder.into_inner().unwrap().finish().unwrap()
}

fn archive_with_raw_path(raw_path: &[u8]) -> Vec<u8> {
    assert!(raw_path.len() < 100);
    let content = b"package source\n";
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    let mut header = tar::Header::new_gnu();
    header.as_mut_bytes()[..raw_path.len()].copy_from_slice(raw_path);
    header.set_size(u64::try_from(content.len()).unwrap());
    header.set_mode(0o644);
    header.set_entry_type(tar::EntryType::Regular);
    header.set_cksum();
    builder.append(&header, Cursor::new(content)).unwrap();
    builder.into_inner().unwrap().finish().unwrap()
}

fn gzip_bytes(bytes: &[u8]) -> Vec<u8> {
    let mut encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    encoder.write_all(bytes).unwrap();
    encoder.finish().unwrap()
}

fn gzip_single_raw_header(header: &tar::Header) -> Vec<u8> {
    gzip_bytes(header.as_bytes())
}

fn corrupt_first_tar_entry_padding(archive: &[u8]) -> Vec<u8> {
    let mut raw = Vec::new();
    flate2::read::GzDecoder::new(archive)
        .read_to_end(&mut raw)
        .unwrap();
    let size = usize::try_from(tar::Header::from_byte_slice(&raw[..512]).size().unwrap()).unwrap();
    let padding_offset = 512 + size;
    let next_header_offset = 512 + size.div_ceil(512) * 512;
    assert!(padding_offset < next_header_offset);
    assert_eq!(raw[padding_offset], 0);
    raw[padding_offset] = 1;
    gzip_bytes(&raw)
}

fn gnu_long_name_archive(paths: &[(&str, Vec<u8>)]) -> Vec<u8> {
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    for (path, payload) in paths {
        let header = canonical_gnu_long_name_header(path.len()).unwrap();
        builder.append(&header, Cursor::new(payload)).unwrap();
    }
    builder.into_inner().unwrap().finish().unwrap()
}

#[derive(Clone)]
struct Fixture {
    module: ModulePath,
    version: ExactVersion,
    files: SourceFiles,
    tree_raw: Vec<u8>,
    source_raw: Vec<u8>,
    release: ReleaseManifest,
    release_raw: Vec<u8>,
    locked: LockedModule,
}

fn fixture_with(
    mod_content: &str,
    extra_files: &[(&str, &[u8])],
    artifacts: Vec<ManifestArtifact>,
) -> Fixture {
    let module = ModulePath::parse("github.com/acme/lib").unwrap();
    let version = ExactVersion::parse("1.2.3").unwrap();
    let mut files = vec![("vo.mod".to_string(), mod_content.as_bytes().to_vec())];
    files.extend(
        extra_files
            .iter()
            .map(|(path, bytes)| ((*path).to_string(), (*bytes).to_vec())),
    );
    files.sort_by(|left, right| left.0.cmp(&right.0));
    let tree_raw = tree_bytes(&files);
    let source_raw = archive_with_package("source", &files, &tree_raw);
    let mod_file = crate::schema::modfile::ModFile::parse_project(mod_content).unwrap();
    let release = ReleaseManifest {
        format: 1,
        module: module.clone(),
        version: version.clone(),
        vo: ToolchainConstraint::parse("0.1.0").unwrap(),
        intent: crate::lock::module_intent_digest(&mod_file).unwrap(),
        dependencies: Vec::new(),
        source: ManifestSource {
            name: "source.tar.gz".to_string(),
            size: u64::try_from(source_raw.len()).unwrap(),
            digest: Digest::from_sha256(&source_raw),
            tree: Digest::from_sha256(&tree_raw),
        },
        artifacts,
    };
    let release_raw = release.render().unwrap().into_bytes();
    let locked = crate::lock::locked_module_from_manifest_raw(&release, &release_raw);
    Fixture {
        module,
        version,
        files,
        tree_raw,
        source_raw,
        release,
        release_raw,
        locked,
    }
}

fn pure_fixture() -> Fixture {
    fixture_with(
        "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"1.2.3\"\nvo = \"0.1.0\"\n",
        &[("lib.vo", b"package lib\nfunc Hello() {}\n")],
        Vec::new(),
    )
}

#[cfg(unix)]
fn executable_fixture() -> Fixture {
    let mut fixture = fixture_with(
        "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"1.2.3\"\nvo = \"0.1.0\"\n",
        &[("tools/configure", b"#!/bin/sh\nexit 0\n")],
        Vec::new(),
    );
    let mut entries = source_entries(&fixture.files);
    entries
        .iter_mut()
        .find(|entry| entry.path == "tools/configure")
        .unwrap()
        .mode = crate::schema::SourceFileMode::Executable;
    fixture.tree_raw = TreeManifest {
        format: 1,
        files: entries,
    }
    .render()
    .unwrap();
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    for (path, bytes) in &fixture.files {
        let mode = if path == "tools/configure" {
            0o755
        } else {
            0o644
        };
        append_archive_file_with_mode(&mut builder, &format!("source/{path}"), bytes, mode);
    }
    append_archive_file(&mut builder, "source/vo.tree.json", &fixture.tree_raw);
    fixture.source_raw = builder.into_inner().unwrap().finish().unwrap();
    fixture.release.source.size = fixture.source_raw.len() as u64;
    fixture.release.source.digest = Digest::from_sha256(&fixture.source_raw);
    fixture.release.source.tree = Digest::from_sha256(&fixture.tree_raw);
    fixture.release_raw = fixture.release.render().unwrap().into_bytes();
    fixture.locked =
        crate::lock::locked_module_from_manifest_raw(&fixture.release, &fixture.release_raw);
    fixture
}

fn native_fixture() -> (Fixture, ManifestArtifact, Vec<u8>) {
    let bytes = b"native-artifact".to_vec();
    let artifact = ManifestArtifact {
        id: ArtifactId {
            kind: "extension-native".to_string(),
            target: "aarch64-apple-darwin".to_string(),
            name: "libdemo.dylib".to_string(),
        },
        size: u64::try_from(bytes.len()).unwrap(),
        digest: Digest::from_sha256(&bytes),
    };
    let fixture = fixture_with(
        concat!(
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"1.2.3\"\n",
            "vo = \"0.1.0\"\n\n",
            "[extension]\n",
            "name = \"demo\"\n\n",
            "[extension.native]\n",
            "targets = [\"aarch64-apple-darwin\"]\n",
        ),
        &[],
        vec![artifact.clone()],
    );
    (fixture, artifact, bytes)
}

fn write_cached_source(cache_root: &Path, fixture: &Fixture) -> PathBuf {
    initialize_cache_root(cache_root);
    let module_dir =
        crate::cache::layout::cache_dir(cache_root, &fixture.locked.path, &fixture.locked.version);
    std::fs::create_dir_all(&module_dir).unwrap();
    for (path, bytes) in &fixture.files {
        let path = module_dir.join(path);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(path, bytes).unwrap();
    }
    std::fs::write(module_dir.join("vo.tree.json"), &fixture.tree_raw).unwrap();
    std::fs::write(module_dir.join("vo.release.json"), &fixture.release_raw).unwrap();
    std::fs::write(
        module_dir.join(VERSION_MARKER),
        format!("{}\n", fixture.locked.version),
    )
    .unwrap();
    std::fs::write(
        module_dir.join(SOURCE_DIGEST_MARKER),
        format!("{}\n", fixture.release.source.digest),
    )
    .unwrap();
    module_dir
}

struct TestRegistry {
    release_raw: Vec<u8>,
    source_raw: Vec<u8>,
    artifacts: BTreeMap<ArtifactId, Vec<u8>>,
    manifest_fetches: AtomicUsize,
    source_fetches: AtomicUsize,
    artifact_fetches: AtomicUsize,
}

impl TestRegistry {
    fn from_fixture(fixture: &Fixture) -> Self {
        Self {
            release_raw: fixture.release_raw.clone(),
            source_raw: fixture.source_raw.clone(),
            artifacts: BTreeMap::new(),
            manifest_fetches: AtomicUsize::new(0),
            source_fetches: AtomicUsize::new(0),
            artifact_fetches: AtomicUsize::new(0),
        }
    }
}

impl Registry for TestRegistry {
    fn list_version_candidates(&self, _module: &ModulePath) -> Result<Vec<ExactVersion>, Error> {
        Ok(Vec::new())
    }

    fn fetch_manifest_raw(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
    ) -> Result<Vec<u8>, Error> {
        self.manifest_fetches.fetch_add(1, AtomicOrdering::Relaxed);
        Ok(self.release_raw.clone())
    }

    fn fetch_source_package(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        _asset_name: &str,
    ) -> Result<Vec<u8>, Error> {
        self.source_fetches.fetch_add(1, AtomicOrdering::Relaxed);
        Ok(self.source_raw.clone())
    }

    fn fetch_artifact(
        &self,
        _module: &ModulePath,
        _version: &ExactVersion,
        artifact: &ArtifactId,
    ) -> Result<Vec<u8>, Error> {
        self.artifact_fetches.fetch_add(1, AtomicOrdering::Relaxed);
        self.artifacts
            .get(artifact)
            .cloned()
            .ok_or_else(|| Error::RegistryError(format!("missing test artifact {artifact}")))
    }
}

#[test]
fn source_package_round_trips_exact_text_and_binary_closure() {
    let files = vec![
        ("assets/data.bin".to_string(), vec![0, 0xff, 7]),
        ("main.vo".to_string(), b"package main\n".to_vec()),
        (
            "vo.mod".to_string(),
            b"format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
                .to_vec(),
        ),
    ];
    let tree_raw = tree_bytes(&files);
    let archive = archive_with_package("source", &files, &tree_raw);
    let extracted = extract_source_entries(&archive).unwrap();
    assert_eq!(extracted.tree_bytes, tree_raw);
    let found = extracted
        .files
        .into_iter()
        .map(|file| (file.path.to_string_lossy().into_owned(), file.bytes))
        .collect::<Vec<_>>();
    assert_eq!(found, files);
}

#[test]
fn source_package_authenticates_regular_and_executable_modes() {
    let mod_bytes =
        b"format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
    let configure = b"#!/bin/sh\nexit 0\n";
    let mut entries = vec![
        SourceFileEntry {
            path: "tools/configure".to_string(),
            mode: crate::schema::SourceFileMode::Executable,
            size: configure.len() as u64,
            digest: Digest::from_sha256(configure),
        },
        SourceFileEntry {
            path: "vo.mod".to_string(),
            mode: crate::schema::SourceFileMode::Regular,
            size: mod_bytes.len() as u64,
            digest: Digest::from_sha256(mod_bytes),
        },
    ];
    entries.sort_by(|left, right| left.path.cmp(&right.path));
    let tree_raw = TreeManifest {
        format: 1,
        files: entries,
    }
    .render()
    .unwrap();
    let archive = archive_with_modes(&[
        ("source/tools/configure", configure, 0o755),
        ("source/vo.mod", mod_bytes, 0o644),
        ("source/vo.tree.json", &tree_raw, 0o644),
    ]);
    let extracted = extract_source_entries(&archive).unwrap();
    assert_eq!(
        extracted
            .files
            .iter()
            .find(|file| file.path == Path::new("tools/configure"))
            .unwrap()
            .mode,
        crate::schema::SourceFileMode::Executable,
    );

    let mismatch = archive_with_modes(&[
        ("source/tools/configure", configure, 0o644),
        ("source/vo.mod", mod_bytes, 0o644),
        ("source/vo.tree.json", &tree_raw, 0o644),
    ]);
    assert!(extract_source_entries(&mismatch)
        .unwrap_err()
        .contains("mode does not match"));

    let noncanonical = archive_with_modes(&[("source/vo.mod", mod_bytes, 0o700)]);
    assert!(extract_source_entries(&noncanonical)
        .unwrap_err()
        .contains("non-canonical mode"));
}

#[test]
fn source_packages_require_the_fixed_source_root() {
    let files = vec![(
        "vo.mod".to_string(),
        b"format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
            .to_vec(),
    )];
    let tree_raw = tree_bytes(&files);
    let archive = archive_with_package("legacy-name-v1.0.0", &files, &tree_raw);

    let error = extract_source_entries(&archive).unwrap_err();

    assert!(
        error.contains("root component must be exactly \"source\""),
        "{error}"
    );
}

#[test]
fn source_archive_text_sizes_are_rejected_before_payload_allocation() {
    let advertised_size = vo_common::vfs::MAX_TEXT_FILE_BYTES + 1;
    for path in ["source/vo.tree.json", "source/src/huge.vo"] {
        let header =
            canonical_source_file_header(path, u64::try_from(advertised_size).unwrap(), 0o644)
                .unwrap();
        let error = extract_source_entries(&gzip_single_raw_header(&header)).unwrap_err();
        assert!(
            error.contains(path.trim_start_matches("source/")),
            "{error}"
        );
        assert!(
            error.contains(&format!(
                "exceeds the {}-byte limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )),
            "{error}",
        );
    }
}

#[test]
fn source_archives_require_regular_files_below_the_fixed_root() {
    let root = "source";
    let files = vec![(
        "vo.mod".to_string(),
        b"format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
            .to_vec(),
    )];
    let tree_raw = tree_bytes(&files);
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::fast());
    let mut builder = tar::Builder::new(encoder);
    let oversized_size = MAX_SOURCE_ARCHIVE_ENTRY_BYTES + 1;
    let mut header = tar::Header::new_gnu();
    header.set_size(u64::try_from(oversized_size).unwrap());
    header.set_mode(0o644);
    header.set_entry_type(tar::EntryType::Regular);
    header.set_cksum();
    let mut oversized_payload = std::io::repeat(b'x').take(u64::try_from(oversized_size).unwrap());
    builder
        .append_data(&mut header, root, &mut oversized_payload)
        .unwrap();
    append_archive_file(&mut builder, &format!("{root}/vo.mod"), &files[0].1);
    append_archive_file(&mut builder, &format!("{root}/vo.tree.json"), &tree_raw);
    let archive = builder.into_inner().unwrap().finish().unwrap();

    let error = extract_source_entries(&archive).unwrap_err();

    assert!(
        error.contains("regular file below the single top-level directory"),
        "{error}"
    );
    assert!(!error.contains("byte limit"), "{error}");

    for directory in [format!("{root}/"), format!("{root}/src/")] {
        let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::fast());
        let mut builder = tar::Builder::new(encoder);
        let mut header = tar::Header::new_gnu();
        header.set_size(0);
        header.set_mode(0o755);
        header.set_entry_type(tar::EntryType::Directory);
        header.set_cksum();
        builder
            .append_data(&mut header, &directory, Cursor::new(Vec::<u8>::new()))
            .unwrap();
        let archive = builder.into_inner().unwrap().finish().unwrap();

        let error = extract_source_entries(&archive).unwrap_err();

        assert!(
            error.contains("regular file below the single top-level directory"),
            "{directory}: {error}"
        );
    }
}

#[test]
fn source_package_rejects_missing_extra_and_tampered_files() {
    let files = vec![
        ("main.vo".to_string(), b"package main\n".to_vec()),
        (
            "vo.mod".to_string(),
            b"format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
                .to_vec(),
        ),
    ];
    let tree_raw = tree_bytes(&files);

    let missing = archive_with_package("source", &files[1..].to_vec(), &tree_raw);
    assert!(extract_source_entries(&missing)
        .unwrap_err()
        .contains("missing file"));

    let mut extra_files = files.clone();
    extra_files.push(("README.md".to_string(), b"extra".to_vec()));
    let extra = archive_with_package("source", &extra_files, &tree_raw);
    assert!(extract_source_entries(&extra)
        .unwrap_err()
        .contains("absent from vo.tree.json"));

    let mut tampered_files = files;
    tampered_files[0].1 = b"package changed\n".to_vec();
    let tampered = archive_with_package("source", &tampered_files, &tree_raw);
    assert!(extract_source_entries(&tampered)
        .unwrap_err()
        .contains("does not match vo.tree.json"));
}

#[test]
fn source_archives_reject_noncanonical_and_non_utf8_wire_paths() {
    for raw_path in [
        b"source/src\\main.vo".as_slice(),
        b"source//main.vo".as_slice(),
        b"C:/main.vo".as_slice(),
        "demo-e\u{301}/main.vo".as_bytes(),
        b"source./main.vo".as_slice(),
    ] {
        let error = extract_source_entries(&archive_with_raw_path(raw_path)).unwrap_err();
        assert!(
            error.contains("not portable") || error.contains("root component"),
            "{}: {error}",
            String::from_utf8_lossy(raw_path),
        );
    }
    let error = extract_source_entries(&archive_with_raw_path(b"source/main-\xff.vo")).unwrap_err();
    assert!(error.contains("valid UTF-8"), "{error}");
}

#[test]
fn source_archives_reject_portable_collisions_and_reserved_paths() {
    let collision = raw_archive(&[
        ("source/Source/main.vo", b"a"),
        ("source/source/other.vo", b"b"),
    ]);
    assert!(extract_source_entries(&collision)
        .unwrap_err()
        .contains("conflicts with portable spelling"));

    for path in [
        "source/VO.RELEASE.JSON",
        "source/artifactſ/demo.wasm",
        "source/.vo-ſource-digest",
    ] {
        let archive = raw_archive(&[(path, b"reserved")]);
        let error = extract_source_entries(&archive).unwrap_err();
        assert!(error.contains("reserved"), "{path}: {error}");
    }
}

#[test]
fn source_packages_share_the_portable_component_budget() {
    let maximum = "a".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES);
    let files = vec![
        (maximum.clone(), Vec::new()),
        (
            "vo.mod".to_string(),
            b"format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
                .to_vec(),
        ),
    ];
    let tree_raw = tree_bytes(&files);
    let archive = archive_with_package("source", &files, &tree_raw);
    assert!(extract_source_entries(&archive)
        .unwrap()
        .files
        .iter()
        .any(|file| file.path == Path::new(&maximum)));

    let oversized = "a".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES + 1);
    let archive = raw_archive(&[(&format!("source/{oversized}"), b"oversized")]);
    assert!(extract_source_entries(&archive)
        .unwrap_err()
        .contains("not portable"));
}

#[test]
fn source_packages_accept_the_maximum_wire_path_through_canonical_gnu_long_name() {
    let mut components = std::iter::repeat_n("a".repeat(255), 15).collect::<Vec<_>>();
    components.push("b".repeat(254));
    components.push("c".to_string());
    let maximum = components.join("/");
    assert_eq!(maximum.len(), crate::schema::MAX_PORTABLE_PATH_BYTES);
    let files = vec![
        (maximum.clone(), Vec::new()),
        (
            "vo.mod".to_string(),
            b"format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
                .to_vec(),
        ),
    ];
    let tree_raw = tree_bytes(&files);
    let archive = archive_with_package("source", &files, &tree_raw);

    let extracted = extract_source_entries(&archive).unwrap();
    assert!(extracted
        .files
        .iter()
        .any(|file| file.path == Path::new(&maximum)));

    let error = extract_source_entries(&corrupt_first_tar_entry_padding(&archive)).unwrap_err();
    assert!(error.contains("tar entry padding must be zero"), "{error}");
}

#[test]
fn raw_tar_rejects_long_name_and_extension_bombs_before_payload_allocation() {
    let mut oversized_long_name =
        canonical_gnu_long_name_header(MAX_GNU_LONG_NAME_PAYLOAD_BYTES).unwrap();
    oversized_long_name.set_cksum();
    let error = extract_source_entries(&gzip_single_raw_header(&oversized_long_name)).unwrap_err();
    assert!(error.contains("GNU long-name payload"), "{error}");

    for entry_type in [
        tar::EntryType::XHeader,
        tar::EntryType::XGlobalHeader,
        tar::EntryType::GNULongLink,
        tar::EntryType::new(b'Z'),
    ] {
        let mut header = tar::Header::new_gnu();
        header.set_path("metadata-bomb").unwrap();
        header.set_size(u64::MAX / 2);
        header.set_mode(0o644);
        header.set_entry_type(entry_type);
        header.set_cksum();
        let error = extract_source_entries(&gzip_single_raw_header(&header)).unwrap_err();
        assert!(error.contains("tar entry type"), "{entry_type:?}: {error}");
    }
}

#[test]
fn raw_tar_rejects_repeated_dangling_and_malformed_gnu_long_names() {
    let wire_path = format!("source/{}", "a".repeat(100));
    let mut payload = wire_path.as_bytes().to_vec();
    payload.push(0);

    let repeated =
        gnu_long_name_archive(&[(&wire_path, payload.clone()), (&wire_path, payload.clone())]);
    let error = extract_source_entries(&repeated).unwrap_err();
    assert!(error.contains("followed immediately"), "{error}");

    let dangling = gnu_long_name_archive(&[(&wire_path, payload.clone())]);
    let error = extract_source_entries(&dangling).unwrap_err();
    assert!(error.contains("not followed by a regular file"), "{error}");

    let mut malformed = payload;
    malformed[8] = 0;
    let malformed = gnu_long_name_archive(&[(&wire_path, malformed)]);
    let error = extract_source_entries(&malformed).unwrap_err();
    assert!(error.contains("exactly one trailing NUL"), "{error}");
}

#[test]
fn source_package_rejects_noncanonical_tar_and_gzip_endings() {
    let files = vec![(
        "vo.mod".to_string(),
        b"format = 1\nmodule = \"github.com/acme/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
            .to_vec(),
    )];
    let tree_raw = tree_bytes(&files);
    let valid = archive_with_package("source", &files, &tree_raw);
    extract_source_entries(&valid).unwrap();

    let error = extract_source_entries(&corrupt_first_tar_entry_padding(&valid)).unwrap_err();
    assert!(error.contains("tar entry padding must be zero"), "{error}");

    let mut corrupt_crc = valid.clone();
    let crc_index = corrupt_crc.len() - 8;
    corrupt_crc[crc_index] ^= 1;
    let error = extract_source_entries(&corrupt_crc).unwrap_err();
    assert!(error.contains("gzip trailer"), "{error}");

    let mut trailing_compressed = valid.clone();
    trailing_compressed.push(0);
    let error = extract_source_entries(&trailing_compressed).unwrap_err();
    assert!(error.contains("trailing compressed bytes"), "{error}");

    let mut concatenated = valid.clone();
    concatenated.extend_from_slice(&valid);
    let error = extract_source_entries(&concatenated).unwrap_err();
    assert!(error.contains("concatenated gzip member"), "{error}");

    let mut tar_bytes = Vec::new();
    flate2::read::GzDecoder::new(valid.as_slice())
        .read_to_end(&mut tar_bytes)
        .unwrap();
    assert!(tar_bytes.len() >= 1024);
    assert!(tar_bytes[tar_bytes.len() - 1024..]
        .iter()
        .all(|byte| *byte == 0));

    let mut nonzero_end = tar_bytes.clone();
    *nonzero_end.last_mut().unwrap() = 1;
    let error = extract_source_entries(&gzip_bytes(&nonzero_end)).unwrap_err();
    assert!(error.contains("nonzero bytes"), "{error}");

    let mut missing_end = tar_bytes.clone();
    missing_end.truncate(missing_end.len() - 512);
    let error = extract_source_entries(&gzip_bytes(&missing_end)).unwrap_err();
    assert!(
        error.contains("missing its canonical tar end block"),
        "{error}"
    );

    let mut extra_end = tar_bytes;
    extra_end.push(0);
    let error = extract_source_entries(&gzip_bytes(&extra_end)).unwrap_err();
    assert!(error.contains("after the canonical tar ending"), "{error}");
}

#[test]
fn archive_scan_enforces_aggregate_complete_path_key_bytes() {
    let deep_path = |branch: usize| {
        let mut components = vec![format!("root{branch:011}")];
        components.extend(std::iter::repeat_n("component000000".to_string(), 255));
        components.join("/")
    };
    let encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    let mut builder = tar::Builder::new(encoder);
    for branch in 0..32 {
        append_archive_file(&mut builder, &format!("source/{}", deep_path(branch)), b"");
    }
    let archive = builder.into_inner().unwrap().finish().unwrap();

    let error = extract_source_entries(&archive).unwrap_err();
    assert!(error.contains("path-key limit"), "{error}");
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn cache_transactions_use_unique_bounded_names_and_exclusive_files() {
    let temp = tempfile::tempdir().unwrap();
    let cache_lock = crate::cache::mutation_lock::CacheMutationLock::shared(temp.path()).unwrap();
    let first = cache_lock.begin_transaction(&"a".repeat(255)).unwrap();
    let second = cache_lock.begin_transaction(&"a".repeat(255)).unwrap();
    assert_ne!(first.relative_path(), second.relative_path());
    assert!(first.relative_path().file_name().unwrap().len() < 128);
    first
        .write_file(Path::new("payload"), b"new-bytes")
        .unwrap();
    assert_eq!(
        first.read_file(Path::new("payload"), 9).unwrap(),
        b"new-bytes"
    );
    assert!(first.write_file(Path::new("payload"), b"again").is_err());
}

#[test]
fn authenticated_cached_source_skips_network_fetch() {
    let temp = tempfile::tempdir().unwrap();
    let fixture = pure_fixture();
    write_cached_source(temp.path(), &fixture);
    let registry = TestRegistry::from_fixture(&fixture);

    download_source(
        temp.path(),
        &fixture.locked,
        &registry,
        &fixture.release,
        &fixture.release_raw,
    )
    .unwrap();

    assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 0);
}

#[cfg(unix)]
#[test]
fn native_cache_installs_and_revalidates_authenticated_executable_mode() {
    use std::os::unix::fs::{MetadataExt as _, PermissionsExt as _};

    let temp = tempfile::tempdir().unwrap();
    initialize_cache_root(temp.path());
    let fixture = executable_fixture();
    let registry = TestRegistry::from_fixture(&fixture);
    download_source(
        temp.path(),
        &fixture.locked,
        &registry,
        &fixture.release,
        &fixture.release_raw,
    )
    .unwrap();

    let executable =
        crate::cache::layout::cache_dir(temp.path(), &fixture.locked.path, &fixture.locked.version)
            .join("tools/configure");
    assert_eq!(
        std::fs::metadata(&executable).unwrap().mode() & 0o777,
        0o700
    );
    let fs = vo_common::vfs::RealFs::new(temp.path());
    validate_source_cache_entry_with_fs(&fs, &fixture.locked).unwrap();

    std::fs::set_permissions(&executable, std::fs::Permissions::from_mode(0o600)).unwrap();
    let error = validate_source_cache_entry_with_fs(&fs, &fixture.locked).unwrap_err();
    assert!(
        error.to_string().contains("executable mode drifted"),
        "{error}"
    );

    std::fs::set_permissions(&executable, std::fs::Permissions::from_mode(0o610)).unwrap();
    let error = validate_source_cache_entry_with_fs(&fs, &fixture.locked).unwrap_err();
    assert!(
        error.to_string().contains("executable mode drifted"),
        "{error}"
    );
}

#[test]
fn tree_binding_failure_precedes_cache_mutation() {
    let temp = tempfile::tempdir().unwrap();
    initialize_cache_root(temp.path());
    let mut fixture = pure_fixture();
    let alternate_package = TreeManifest {
        format: 1,
        files: vec![SourceFileEntry {
            path: "vo.mod".to_string(),
            mode: crate::schema::SourceFileMode::Regular,
            size: 1,
            digest: Digest::from_sha256(b"x"),
        }],
    }
    .render()
    .unwrap();
    fixture.release.source.tree = Digest::from_sha256(&alternate_package);
    fixture.release_raw = fixture.release.render().unwrap().into_bytes();
    fixture.locked =
        crate::lock::locked_module_from_manifest_raw(&fixture.release, &fixture.release_raw);
    let registry = TestRegistry::from_fixture(&fixture);

    let error = download_source(
        temp.path(),
        &fixture.locked,
        &registry,
        &fixture.release,
        &fixture.release_raw,
    )
    .unwrap_err();
    assert!(matches!(error, Error::DigestMismatch { .. }), "{error}");
    assert!(
        !crate::cache::layout::cache_dir(temp.path(), &fixture.module, &fixture.version,).exists()
    );
}

#[test]
fn invalid_existing_artifact_is_preserved() {
    let temp = tempfile::tempdir().unwrap();
    let (fixture, artifact, artifact_bytes) = native_fixture();
    let module_dir = write_cached_source(temp.path(), &fixture);
    let artifact_path =
        module_dir.join(crate::artifact::artifact_relative_path(&artifact.id).unwrap());
    std::fs::create_dir_all(artifact_path.parent().unwrap()).unwrap();
    std::fs::write(&artifact_path, b"stale").unwrap();
    let mut registry = TestRegistry::from_fixture(&fixture);
    registry
        .artifacts
        .insert(artifact.id.clone(), artifact_bytes);

    let error = download_artifact(temp.path(), &fixture.locked, &artifact, &registry).unwrap_err();
    assert!(
        error.to_string().contains("clean the module cache"),
        "{error}"
    );
    assert_eq!(registry.artifact_fetches.load(AtomicOrdering::Relaxed), 0);
    assert_eq!(std::fs::read(artifact_path).unwrap(), b"stale");
}

#[test]
fn invalid_existing_source_fails_before_registry_access() {
    let temp = tempfile::tempdir().unwrap();
    let fixture = pure_fixture();
    let module_dir = write_cached_source(temp.path(), &fixture);
    std::fs::write(module_dir.join("lib.vo"), b"tampered").unwrap();
    let registry = TestRegistry::from_fixture(&fixture);

    let error = populate_locked_modules(
        temp.path(),
        std::slice::from_ref(&fixture.locked),
        &registry,
    )
    .unwrap_err();

    assert!(
        error.to_string().contains("clean the module cache"),
        "{error}"
    );
    assert_eq!(registry.manifest_fetches.load(AtomicOrdering::Relaxed), 0);
    assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 0);
    assert_eq!(
        std::fs::read(module_dir.join("lib.vo")).unwrap(),
        b"tampered"
    );
}

#[test]
fn cached_release_and_package_tampering_are_detected() {
    for target in ["vo.release.json", "vo.tree.json", "lib.vo"] {
        let temp = tempfile::tempdir().unwrap();
        let fixture = pure_fixture();
        let module_dir = write_cached_source(temp.path(), &fixture);
        std::fs::write(module_dir.join(target), b"tampered").unwrap();
        let read_lock =
            crate::cache::mutation_lock::CacheMutationLock::shared(temp.path()).unwrap();
        assert!(
            validate_source_cache_entry_with_fs(&read_lock.file_system(), &fixture.locked).is_err()
        );
    }
}

#[test]
fn locked_install_materializes_authenticated_protocol_objects() {
    let temp = tempfile::tempdir().unwrap();
    let fixture = pure_fixture();
    let registry = TestRegistry::from_fixture(&fixture);
    populate_locked_modules(
        temp.path(),
        std::slice::from_ref(&fixture.locked),
        &registry,
    )
    .unwrap();
    let cache_dir =
        crate::cache::layout::cache_dir(temp.path(), &fixture.locked.path, &fixture.locked.version);

    assert_eq!(registry.source_fetches.load(AtomicOrdering::Relaxed), 1);
    assert_eq!(
        std::fs::read(cache_dir.join("vo.tree.json")).unwrap(),
        fixture.tree_raw,
    );
    assert_eq!(
        std::fs::read_to_string(cache_dir.join(SOURCE_DIGEST_MARKER))
            .unwrap()
            .trim(),
        fixture.release.source.digest.to_string(),
    );
    assert_eq!(
        std::fs::read_to_string(cache_dir.join(VERSION_MARKER))
            .unwrap()
            .trim(),
        "1.2.3",
    );
}

#[test]
fn install_rejects_extension_artifact_contract_mismatch() {
    let temp = tempfile::tempdir().unwrap();
    let fixture = fixture_with(
        concat!(
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"1.2.3\"\n",
            "vo = \"0.1.0\"\n\n",
            "[extension.wasm]\n",
            "kind = \"standalone\"\n",
            "wasm = \"lib.wasm\"\n\n",
            "[extension]\n",
            "name = \"lib\"\n",
        ),
        &[],
        Vec::new(),
    );
    let registry = TestRegistry::from_fixture(&fixture);
    let error = populate_locked_modules(
        temp.path(),
        std::slice::from_ref(&fixture.locked),
        &registry,
    )
    .unwrap_err();
    assert!(matches!(error, Error::InvalidReleaseMetadata(_)), "{error}");
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn committed_source_and_artifact_durability_failures_are_reported() {
    let source_root = tempfile::tempdir().unwrap();
    let fixture = pure_fixture();
    let registry = TestRegistry::from_fixture(&fixture);
    let destination =
        crate::cache::layout::cache_dir(source_root.path(), &fixture.module, &fixture.version);
    crate::cache::mutation_lock::fail_publication_sync_for_test(&destination);
    let error = populate_locked_modules(
        source_root.path(),
        std::slice::from_ref(&fixture.locked),
        &registry,
    )
    .unwrap_err();
    assert!(matches!(
        error,
        Error::CachePublicationDurabilityUnconfirmed { .. }
    ));
    let read_lock =
        crate::cache::mutation_lock::CacheMutationLock::shared(source_root.path()).unwrap();
    validate_source_cache_entry_with_fs(&read_lock.file_system(), &fixture.locked).unwrap();

    let artifact_root = tempfile::tempdir().unwrap();
    let (fixture, artifact, artifact_bytes) = native_fixture();
    let module_dir = write_cached_source(artifact_root.path(), &fixture);
    let artifact_path =
        module_dir.join(crate::artifact::artifact_relative_path(&artifact.id).unwrap());
    crate::cache::mutation_lock::fail_publication_sync_for_test(&artifact_path);
    let mut registry = TestRegistry::from_fixture(&fixture);
    registry
        .artifacts
        .insert(artifact.id.clone(), artifact_bytes);
    let error =
        download_artifact(artifact_root.path(), &fixture.locked, &artifact, &registry).unwrap_err();
    assert!(matches!(
        error,
        Error::CachePublicationDurabilityUnconfirmed { .. }
    ));
    let read_lock =
        crate::cache::mutation_lock::CacheMutationLock::shared(artifact_root.path()).unwrap();
    validate_artifact_cache_entry_with_fs(&read_lock.file_system(), &fixture.locked, &artifact)
        .unwrap();
}

#[cfg(unix)]
#[test]
fn source_install_rejects_symlinked_cache_components_without_touching_targets() {
    use std::os::unix::fs::symlink;

    for symlink_version in [false, true] {
        let root = tempfile::tempdir().unwrap();
        initialize_cache_root(root.path());
        let outside = tempfile::tempdir().unwrap();
        let sentinel = outside.path().join("keep.vo");
        std::fs::write(&sentinel, b"package keep\n").unwrap();
        let fixture = pure_fixture();
        let cache_key = crate::cache::layout::cache_key(&fixture.module);
        if symlink_version {
            std::fs::create_dir(root.path().join(&cache_key)).unwrap();
            symlink(
                outside.path(),
                root.path()
                    .join(&cache_key)
                    .join(fixture.version.to_string()),
            )
            .unwrap();
        } else {
            symlink(outside.path(), root.path().join(&cache_key)).unwrap();
        }
        let registry = TestRegistry::from_fixture(&fixture);
        let error = populate_locked_modules(
            root.path(),
            std::slice::from_ref(&fixture.locked),
            &registry,
        )
        .unwrap_err();
        assert!(
            error.to_string().contains("symbolic link") || error.to_string().contains("Symlink"),
            "{error}",
        );
        assert_eq!(std::fs::read(&sentinel).unwrap(), b"package keep\n");
    }
}
