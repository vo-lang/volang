use serde::{Deserialize, Serialize};

use crate::digest::Digest;

use super::{is_reserved_module_cache_path, validate_portable_relative_path, PortablePathSet};

/// Canonical source-file mode authenticated by `vo.package.json`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SourceFileMode {
    Regular,
    Executable,
}

impl SourceFileMode {
    pub fn is_executable(self) -> bool {
        self == Self::Executable
    }
}

/// One file in the exact source-package closure described by
/// `vo.package.json`.
///
/// Entries describe raw bytes. Text and binary files therefore share the same
/// protocol and integrity checks.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceFileEntry {
    pub path: String,
    pub mode: SourceFileMode,
    pub size: u64,
    pub digest: Digest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PackageFileSet {
    pub total_size: u64,
    /// Source files plus the distinct parent directories that must be
    /// materialized for them.
    pub materialized_entries: usize,
}

/// Fixed entries which may coexist with package-owned children at the module
/// cache root: the artifact directory, two cache markers, and the release and
/// package manifests.
pub(crate) const CACHE_OWNED_ROOT_ENTRY_BUDGET: usize = 5;

#[derive(Debug, Clone, Copy)]
struct PackageFileSetLimits {
    max_files: usize,
    max_materialized_entries: usize,
    max_root_source_entries: usize,
    max_file_size: u64,
    max_total_size: u64,
}

/// Return whether a portable path may be owned by a source package.
///
/// Cache metadata, artifacts, fixed protocol files, workspace control, and
/// generated transaction/checksum files are reserved at the package root.
/// A nested `vo.mod` is also excluded because it starts a separate module
/// boundary. Other protocol-looking basenames below ordinary directories are
/// source data. Matching follows portable case-folding rules so an archive has
/// one meaning on every supported host.
pub fn is_package_file_candidate(path: &str) -> Result<bool, String> {
    validate_portable_relative_path(path)?;
    if is_reserved_module_cache_path(path) {
        return Ok(false);
    }
    let file_name = path.rsplit('/').next().unwrap_or(path);
    let file_key = super::portable_case_key(file_name);
    if file_key == "vo.mod" {
        return Ok(path == "vo.mod");
    }
    if path.contains('/') {
        return Ok(true);
    }
    Ok(!matches!(
        file_key.as_str(),
        "vo.lock"
            | "vo.work"
            | "vo.sum"
            | "vo.web.json"
            | ".vo-project.lock"
            | ".vo-project.transaction"
    ))
}

/// Validate the complete, canonical source-package file closure.
///
/// Callers remain responsible for requiring lexical ordering on untrusted
/// wire input. This helper checks portable-path uniqueness, file bounds, and
/// the aggregate extracted-size bound while accepting arbitrary raw bytes.
pub fn validate_package_file_set(entries: &[SourceFileEntry]) -> Result<PackageFileSet, String> {
    validate_package_file_set_with_limits(
        entries,
        PackageFileSetLimits {
            max_files: crate::MAX_SOURCE_ARCHIVE_ENTRIES.saturating_sub(1),
            max_materialized_entries: crate::MAX_SOURCE_ARCHIVE_ENTRIES,
            max_root_source_entries: vo_common::vfs::MAX_DIRECTORY_ENTRIES
                .saturating_sub(CACHE_OWNED_ROOT_ENTRY_BUDGET),
            max_file_size: u64::try_from(crate::MAX_SOURCE_ARCHIVE_ENTRY_BYTES).unwrap_or(u64::MAX),
            max_total_size: u64::try_from(crate::MAX_EXTRACTED_SOURCE_BYTES).unwrap_or(u64::MAX),
        },
    )
}

fn validate_package_file_set_with_limits(
    entries: &[SourceFileEntry],
    limits: PackageFileSetLimits,
) -> Result<PackageFileSet, String> {
    if entries.len() > limits.max_files {
        return Err(format!(
            "package contains more than {} files (one archive entry is reserved for vo.package.json)",
            limits.max_files,
        ));
    }
    if entries.len() > limits.max_materialized_entries {
        return Err(format!(
            "package files require more than {} materialized source-tree entries",
            limits.max_materialized_entries,
        ));
    }

    let mut paths = PortablePathSet::default();
    let mut total_size = 0u64;

    for (index, entry) in entries.iter().enumerate() {
        validate_portable_relative_path(&entry.path)
            .map_err(|error| format!("files[{index}].path {:?}: {error}", entry.path))?;
        if !is_package_file_candidate(&entry.path)? {
            return Err(format!(
                "files[{index}].path {:?} is reserved by the module protocol",
                entry.path
            ));
        }
        if !paths.insert_file(&entry.path)? {
            return Err(format!(
                "files[{index}].path {:?} is duplicated",
                entry.path
            ));
        }

        if paths.root_materialized_entry_count() > limits.max_root_source_entries {
            return Err(format!(
                "package materializes more than {} source entries at the cache root; {} entries are reserved for cache metadata",
                limits.max_root_source_entries, CACHE_OWNED_ROOT_ENTRY_BUDGET,
            ));
        }
        if paths.materialized_entry_count() > limits.max_materialized_entries {
            return Err(format!(
                "package files require more than {} materialized source-tree entries after implicit directories are created",
                limits.max_materialized_entries,
            ));
        }

        if entry.size > limits.max_file_size {
            return Err(format!(
                "files[{index}] {:?} is {} bytes, exceeding the {}-byte archive-entry limit",
                entry.path, entry.size, limits.max_file_size,
            ));
        }
        if (entry.path == "vo.mod" || entry.path.ends_with(".vo"))
            && entry.size > u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap_or(u64::MAX)
        {
            return Err(format!(
                "files[{index}] {:?} is {} bytes, exceeding the {}-byte Vo text-file limit",
                entry.path,
                entry.size,
                vo_common::vfs::MAX_TEXT_FILE_BYTES,
            ));
        }
        total_size = total_size
            .checked_add(entry.size)
            .ok_or_else(|| "package file size total overflows u64".to_string())?;
        if total_size > limits.max_total_size {
            return Err(format!(
                "package files exceed the {}-byte extracted-source limit",
                limits.max_total_size,
            ));
        }
    }

    Ok(PackageFileSet {
        total_size,
        materialized_entries: paths.materialized_entry_count(),
    })
}

/// Produce a bounded explanation for two unequal authenticated file tables.
/// Only the first differing entry is rendered, keeping corrupt-cache and
/// transport diagnostics independent of the package file count.
pub(crate) fn package_file_set_mismatch_detail(
    expected: &[SourceFileEntry],
    found: &[SourceFileEntry],
) -> String {
    let mismatch_index = expected
        .iter()
        .zip(found)
        .position(|(expected, found)| expected != found)
        .unwrap_or_else(|| expected.len().min(found.len()));
    let expected_entry = expected
        .get(mismatch_index)
        .map(describe_file_entry)
        .unwrap_or_else(|| "<end of file table>".to_string());
    let found_entry = found
        .get(mismatch_index)
        .map(describe_file_entry)
        .unwrap_or_else(|| "<end of file table>".to_string());
    format!(
        "expected {} files, found {}; first mismatch at index {mismatch_index}: expected {expected_entry}, found {found_entry}",
        expected.len(),
        found.len(),
    )
}

fn describe_file_entry(entry: &SourceFileEntry) -> String {
    format!(
        "{:?} ({:?}, {} bytes, {})",
        entry.path, entry.mode, entry.size, entry.digest,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(path: &str, bytes: &[u8]) -> SourceFileEntry {
        SourceFileEntry {
            path: path.to_string(),
            mode: SourceFileMode::Regular,
            size: u64::try_from(bytes.len()).unwrap(),
            digest: Digest::from_sha256(bytes),
        }
    }

    #[test]
    fn validates_text_and_binary_entries() {
        let entries = [
            entry("asset.bin", &[0, 0xff, 1]),
            entry("vo.mod", b"module = {}"),
        ];
        let validated = validate_package_file_set(&entries).unwrap();
        assert_eq!(validated.total_size, 14);
        assert_eq!(validated.materialized_entries, 2);
    }

    #[test]
    fn vo_text_declarations_respect_the_real_text_consumer_limit() {
        let oversized = u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .unwrap()
            .checked_add(1)
            .unwrap();
        for path in ["vo.mod", "src/main.vo"] {
            let mut file = entry(path, b"x");
            file.size = oversized;
            let error = validate_package_file_set(&[file]).unwrap_err();
            assert!(error.contains("Vo text-file limit"), "{path}: {error}");
        }

        let mut binary = entry("assets/payload.bin", b"x");
        binary.size = oversized;
        assert!(validate_package_file_set(&[binary]).is_ok());
    }

    #[test]
    fn rejects_protocol_paths_and_portable_collisions() {
        for path in [
            "vo.package.json",
            "VO.PACKAGE.JSON",
            "source.tar.gz",
            "SOURCE.TAR.GZ",
            "ſource.tar.gz",
            "vo.release.json",
            "artifacts/demo.wasm",
            ".vo-version",
            "vo.work",
            "vo.lock",
            "vo.web.json",
            "VO.MOD",
            "nested/vo.mod",
            "nested/VO.MOD",
        ] {
            assert!(!is_package_file_candidate(path).unwrap(), "{path}");
        }
        assert!(is_package_file_candidate("vo.mod").unwrap());
        for path in [
            "docs/vo.package.json",
            "docs/source.tar.gz",
            "nested/VO.LOCK",
            "nested/VO.WEB.JSON",
            "nested/vo.sum",
            "nested/.VO-PROJECT.LOCK",
            "nested/.VO-PROJECT.TRANSACTION",
            "nested/vo.work",
        ] {
            assert!(is_package_file_candidate(path).unwrap(), "{path}");
        }

        let entries = [entry("Straße.bin", b"a"), entry("STRASSE.bin", b"b")];
        let error = validate_package_file_set(&entries).unwrap_err();
        assert!(
            error.contains("conflicts with portable spelling"),
            "{error}"
        );
    }

    #[test]
    fn rejects_file_directory_collisions() {
        let entries = [entry("assets", b"a"), entry("assets/logo.bin", b"b")];
        let error = validate_package_file_set(&entries).unwrap_err();
        assert!(error.contains("descends through file"), "{error}");
    }

    #[test]
    fn accounts_for_implicit_directories_and_cache_root_entries() {
        let shared_directory = [entry("assets/a.bin", b"a"), entry("assets/b.bin", b"b")];
        let validated = validate_package_file_set_with_limits(
            &shared_directory,
            PackageFileSetLimits {
                max_files: 3,
                max_materialized_entries: 3,
                max_root_source_entries: 1,
                max_file_size: 10,
                max_total_size: 10,
            },
        )
        .unwrap();
        assert_eq!(validated.materialized_entries, 3);

        let distinct_directories = [entry("a/one.vo", b"a"), entry("b/two.vo", b"b")];
        let error = validate_package_file_set_with_limits(
            &distinct_directories,
            PackageFileSetLimits {
                max_files: 3,
                max_materialized_entries: 3,
                max_root_source_entries: 2,
                max_file_size: 10,
                max_total_size: 10,
            },
        )
        .unwrap_err();
        assert!(error.contains("implicit directories"), "{error}");

        let root_files = [entry("a.vo", b"a"), entry("b.vo", b"b")];
        let error = validate_package_file_set_with_limits(
            &root_files,
            PackageFileSetLimits {
                max_files: 3,
                max_materialized_entries: 3,
                max_root_source_entries: 1,
                max_file_size: 10,
                max_total_size: 10,
            },
        )
        .unwrap_err();
        assert!(error.contains("cache root"), "{error}");
    }

    #[test]
    fn deep_paths_are_charged_once_per_materialized_trie_node() {
        let shared_parent = (0..(super::super::MAX_PORTABLE_PATH_COMPONENTS - 1))
            .map(|index| format!("d{index:03}"))
            .collect::<Vec<_>>()
            .join("/");
        let shared = [
            entry(&format!("{shared_parent}/a.vo"), b"a"),
            entry(&format!("{shared_parent}/b.vo"), b"b"),
        ];
        let expected_nodes = super::super::MAX_PORTABLE_PATH_COMPONENTS + 1;
        let validated = validate_package_file_set_with_limits(
            &shared,
            PackageFileSetLimits {
                max_files: 2,
                max_materialized_entries: expected_nodes,
                max_root_source_entries: 1,
                max_file_size: 1,
                max_total_size: 2,
            },
        )
        .unwrap();
        assert_eq!(validated.materialized_entries, expected_nodes);

        let disjoint_parent = (0..(super::super::MAX_PORTABLE_PATH_COMPONENTS - 1))
            .map(|index| format!("x{index:03}"))
            .collect::<Vec<_>>()
            .join("/");
        let disjoint = [
            entry(&format!("{shared_parent}/a.vo"), b"a"),
            entry(&format!("{disjoint_parent}/b.vo"), b"b"),
        ];
        let error = validate_package_file_set_with_limits(
            &disjoint,
            PackageFileSetLimits {
                max_files: 2,
                max_materialized_entries: expected_nodes,
                max_root_source_entries: 2,
                max_file_size: 1,
                max_total_size: 2,
            },
        )
        .unwrap_err();
        assert!(error.contains("implicit directories"), "{error}");
    }

    #[test]
    fn mismatch_diagnostic_renders_only_the_first_difference() {
        let expected = [
            entry("first.vo", b"expected"),
            entry("later-secret-sentinel.vo", b"later"),
        ];
        let found = [
            entry("first.vo", b"found"),
            entry("another-later-sentinel.vo", b"later"),
        ];

        let detail = package_file_set_mismatch_detail(&expected, &found);

        assert!(detail.contains("first mismatch at index 0"), "{detail}");
        assert!(detail.contains("first.vo"), "{detail}");
        assert!(!detail.contains("later-secret-sentinel"), "{detail}");
        assert!(!detail.contains("another-later-sentinel"), "{detail}");
    }
}
