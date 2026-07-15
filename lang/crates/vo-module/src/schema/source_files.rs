use serde::{Deserialize, Serialize};

use crate::digest::Digest;

use super::{validate_portable_relative_path, PortablePathSet};

/// Return whether a portable installed path participates in the canonical
/// UTF-8 source-file set.
///
/// Cache-owned metadata, the artifact subtree, and `vo.web.json` are verified
/// by their own protocols. Every other bounded UTF-8 file is source-set data.
/// Case-insensitive protocol-name matching keeps behavior stable on hosts that
/// alias file-name case.
pub fn is_source_file_set_candidate(path: &str) -> Result<bool, String> {
    validate_portable_relative_path(path)?;
    Ok(!super::is_reserved_module_cache_path(path)
        && super::portable_case_key(path) != "vo.web.json")
}

/// One canonical browser-readable source file metadata entry.
///
/// Field declaration order is part of the digest protocol. Serde serializes
/// structs in declaration order, yielding exactly `path,size,digest`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceFileEntry {
    pub path: String,
    pub size: u64,
    pub digest: Digest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CanonicalSourceFileSet {
    pub total_size: u64,
    pub digest: Digest,
    pub bytes: Vec<u8>,
}

/// Sort, validate, and encode a source file set using the release protocol.
pub fn canonical_source_file_set(
    entries: &[SourceFileEntry],
) -> Result<CanonicalSourceFileSet, String> {
    if entries.len() > vo_common::vfs::MAX_PACKAGE_SOURCE_FILES {
        return Err(format!(
            "source file set contains more than {} entries",
            vo_common::vfs::MAX_PACKAGE_SOURCE_FILES
        ));
    }

    let mut canonical = Vec::new();
    canonical
        .try_reserve(entries.len())
        .map_err(|_| "failed to reserve canonical source file entries".to_string())?;
    let mut paths = PortablePathSet::default();
    let mut total_size = 0u64;
    let max_file_size = u64::try_from(vo_common::vfs::MAX_TEXT_FILE_BYTES).unwrap_or(u64::MAX);
    let max_total_size =
        u64::try_from(vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES).unwrap_or(u64::MAX);

    for entry in entries {
        validate_portable_relative_path(&entry.path)
            .map_err(|error| format!("source file path {:?}: {error}", entry.path))?;
        if !paths.insert_file(&entry.path)? {
            return Err(format!(
                "source file set contains duplicate path {:?}",
                entry.path
            ));
        }
        if entry.size > max_file_size {
            return Err(format!(
                "source file {:?} is {} bytes, exceeding the {}-byte text limit",
                entry.path,
                entry.size,
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            ));
        }
        total_size = total_size
            .checked_add(entry.size)
            .ok_or_else(|| "source file set size overflows u64".to_string())?;
        if total_size > max_total_size {
            return Err(format!(
                "source file set exceeds the {}-byte package limit",
                vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES
            ));
        }
        canonical.push(entry.clone());
    }

    canonical.sort_by(|left, right| left.path.cmp(&right.path));
    let bytes = serde_json::to_vec(&canonical)
        .map_err(|error| format!("failed to encode canonical source file set: {error}"))?;
    Ok(CanonicalSourceFileSet {
        total_size,
        digest: Digest::from_sha256(&bytes),
        bytes,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_bytes_and_digest_are_stable_and_field_ordered() {
        let entries = [
            SourceFileEntry {
                path: "vo.mod".to_string(),
                size: 3,
                digest: Digest::from_sha256(b"mod"),
            },
            SourceFileEntry {
                path: "empty.vo".to_string(),
                size: 0,
                digest: Digest::from_sha256(b""),
            },
        ];

        let canonical = canonical_source_file_set(&entries).unwrap();
        assert_eq!(canonical.total_size, 3);
        assert_eq!(
            String::from_utf8(canonical.bytes).unwrap(),
            concat!(
                "[{\"path\":\"empty.vo\",\"size\":0,\"digest\":\"",
                "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
                "\"},{\"path\":\"vo.mod\",\"size\":3,\"digest\":\"",
                "sha256:e55cffc81a5ad8cfe85239d944a3ae9513645a9eed79bc884f51b80b2760fc46",
                "\"}]"
            )
        );
        assert_eq!(
            canonical.digest,
            Digest::parse(
                "sha256:99e9a1fb7265851bf1de4e0be09198396256ba485afacf74c0c54eeb836afa50"
            )
            .unwrap()
        );
    }

    #[test]
    fn only_the_exact_root_web_manifest_is_excluded_from_source_data() {
        assert!(!is_source_file_set_candidate("vo.web.json").unwrap());
        assert!(!is_source_file_set_candidate("VO.WEB.JSON").unwrap());
        assert!(!is_source_file_set_candidate("vo.web.jſon").unwrap());
        assert!(is_source_file_set_candidate("nested/vo.web.json").unwrap());
        assert!(is_source_file_set_candidate("nested/VO.WEB.JSON").unwrap());
        assert!(is_source_file_set_candidate("nested/vo.web.jſon").unwrap());
    }

    #[test]
    fn source_file_set_rejects_multi_code_point_case_fold_aliases() {
        let empty_digest = Digest::from_sha256(b"");
        let entries = vec![
            SourceFileEntry {
                path: "Straße.vo".to_string(),
                size: 0,
                digest: empty_digest.clone(),
            },
            SourceFileEntry {
                path: "STRASSE.vo".to_string(),
                size: 0,
                digest: empty_digest,
            },
        ];

        let error = canonical_source_file_set(&entries).unwrap_err();
        assert!(
            error.contains("conflicts with portable spelling"),
            "{error}"
        );
    }
}
