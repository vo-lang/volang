use serde::Deserialize;

use crate::Error;

use super::{validate_package_file_set, SourceFileEntry, SourceFileMode};

/// Exact source-package file closure stored in `vo.tree.json`.
///
/// The bytes are embedded once at `source/vo.tree.json` inside the archive.
/// Each listed digest covers raw file bytes, so text and binary sources share
/// the same integrity model.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TreeManifest {
    pub format: u64,
    pub files: Vec<SourceFileEntry>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawTreeManifest {
    format: u64,
    #[serde(deserialize_with = "deserialize_package_files")]
    files: Vec<SourceFileEntry>,
}

fn deserialize_package_files<'de, D>(deserializer: D) -> Result<Vec<SourceFileEntry>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    super::deserialize_bounded_vec(
        deserializer,
        crate::MAX_SOURCE_ARCHIVE_ENTRIES.saturating_sub(1),
        "vo.tree.json files",
    )
}

fn invalid(detail: impl Into<String>) -> Error {
    Error::ManifestParse(format!("vo.tree.json: {}", detail.into()))
}

fn validate_sorted_files(files: &[SourceFileEntry]) -> Result<(), Error> {
    if files.windows(2).any(|pair| pair[0].path >= pair[1].path) {
        return Err(invalid("files must be unique and sorted by path"));
    }
    Ok(())
}

fn validate_files(files: &[SourceFileEntry]) -> Result<u64, Error> {
    validate_sorted_files(files)?;
    let file_set = validate_package_file_set(files).map_err(invalid)?;
    let Some(mod_file) = files.iter().find(|entry| entry.path == "vo.mod") else {
        return Err(invalid("files must contain the root vo.mod file"));
    };
    if mod_file.size == 0 {
        return Err(invalid("vo.mod must not be empty"));
    }
    if mod_file.mode != SourceFileMode::Regular {
        return Err(invalid("the root vo.mod mode must be regular"));
    }
    Ok(file_set.total_size)
}

impl TreeManifest {
    pub fn parse(bytes: &[u8]) -> Result<Self, Error> {
        if bytes.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(invalid(format!(
                "exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        let raw: RawTreeManifest = serde_json::from_slice(bytes)
            .map_err(|error| invalid(format!("JSON parse error: {error}")))?;
        if raw.format != 1 {
            return Err(invalid(format!("unsupported format {}", raw.format)));
        }
        let total_size = validate_files(&raw.files)?;
        let archive_size = total_size
            .checked_add(u64::try_from(bytes.len()).unwrap_or(u64::MAX))
            .ok_or_else(|| invalid("extracted byte size overflows u64"))?;
        let max_extracted = u64::try_from(crate::MAX_EXTRACTED_SOURCE_BYTES).unwrap_or(u64::MAX);
        if archive_size > max_extracted {
            return Err(invalid(format!(
                "files plus vo.tree.json exceed the {max_extracted}-byte extracted-source limit"
            )));
        }

        let manifest = Self {
            format: raw.format,
            files: raw.files,
        };
        let canonical = manifest.render()?;
        if canonical != bytes {
            return Err(invalid("JSON must use the canonical encoding"));
        }
        Ok(manifest)
    }

    fn validate_semantics(&self) -> Result<u64, Error> {
        if self.format != 1 {
            return Err(invalid(format!("unsupported format {}", self.format)));
        }
        validate_files(&self.files)
    }

    /// Revalidate a typed value constructed by Rust code, including the
    /// canonical wire-size and extracted-source ceilings.
    pub fn validate(&self) -> Result<(), Error> {
        self.render().map(|_| ())
    }

    /// Serialize the complete canonical asset bytes in field order with one
    /// trailing LF. Callers bind and embed this byte sequence unchanged.
    pub fn render(&self) -> Result<Vec<u8>, Error> {
        let total_size = self.validate_semantics()?;
        let mut output = super::CanonicalJsonWriter::new(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(invalid)?;

        macro_rules! raw {
            ($value:expr) => {
                output.push_raw($value).map_err(invalid)?
            };
        }
        macro_rules! string {
            ($value:expr) => {
                output.push_string($value).map_err(invalid)?
            };
        }
        macro_rules! number {
            ($value:expr) => {
                output.push_u64($value).map_err(invalid)?
            };
        }

        raw!("{\n  \"format\": ");
        number!(self.format);
        if self.files.is_empty() {
            raw!(",\n  \"files\": []\n}\n");
        } else {
            raw!(",\n  \"files\": [\n");
            for (index, entry) in self.files.iter().enumerate() {
                raw!("    {\n      \"path\": ");
                string!(&entry.path);
                raw!(",\n      \"mode\": ");
                let mode = match entry.mode {
                    super::SourceFileMode::Regular => "regular",
                    super::SourceFileMode::Executable => "executable",
                };
                string!(mode);
                raw!(",\n      \"size\": ");
                number!(entry.size);
                raw!(",\n      \"digest\": ");
                string!(entry.digest.as_str());
                raw!("\n    }");
                if index + 1 != self.files.len() {
                    raw!(",");
                }
                raw!("\n");
            }
            raw!("  ]\n}\n");
        }
        let bytes = output.finish();
        let archive_size = total_size
            .checked_add(u64::try_from(bytes.len()).unwrap_or(u64::MAX))
            .ok_or_else(|| invalid("extracted byte size overflows u64"))?;
        let max_extracted = u64::try_from(crate::MAX_EXTRACTED_SOURCE_BYTES).unwrap_or(u64::MAX);
        if archive_size > max_extracted {
            return Err(invalid(format!(
                "files plus vo.tree.json exceed the {max_extracted}-byte extracted-source limit"
            )));
        }
        Ok(bytes)
    }
}

#[cfg(test)]
mod tests {
    use crate::digest::Digest;

    use super::*;

    fn entry_with_mode(path: &str, mode: SourceFileMode, bytes: &[u8]) -> SourceFileEntry {
        SourceFileEntry {
            path: path.to_string(),
            mode,
            size: u64::try_from(bytes.len()).unwrap(),
            digest: Digest::from_sha256(bytes),
        }
    }

    fn entry(path: &str, bytes: &[u8]) -> SourceFileEntry {
        entry_with_mode(path, SourceFileMode::Regular, bytes)
    }

    fn sample() -> TreeManifest {
        TreeManifest {
            format: 1,
            files: vec![
                entry("assets/data.bin", &[0, 0xff, 7]),
                entry("assets/\u{e000}.bin", b"bmp"),
                entry("assets/\u{10000}.bin", b"supplementary"),
                entry("src/main.vo", b"fn main() {}"),
                entry_with_mode(
                    "tools/generate",
                    SourceFileMode::Executable,
                    b"#!/bin/sh\nexit 0\n",
                ),
                entry(
                    "vo.mod",
                    b"module = 'github.com/acme/demo'\nvo = '^1.0.0'\n",
                ),
            ],
        }
    }

    fn sample_json() -> Vec<u8> {
        r#"{
  "format": 1,
  "files": [
    {
      "path": "assets/data.bin",
      "mode": "regular",
      "size": 3,
      "digest": "sha256:92e469e6f34332f611f46cc5371592264628458db80d2a9c40310daec8384d23"
    },
    {
      "path": "assets/.bin",
      "mode": "regular",
      "size": 3,
      "digest": "sha256:7685a5cb2b57c5026f3bcb223275c917ffcaf8c7a89262d68689e0c84802f230"
    },
    {
      "path": "assets/𐀀.bin",
      "mode": "regular",
      "size": 13,
      "digest": "sha256:f6c48b6b20435f3d1fcb7013105e68c1fd58deab167329bf35511ad2f0bfa446"
    },
    {
      "path": "src/main.vo",
      "mode": "regular",
      "size": 12,
      "digest": "sha256:ef32637cb9c3ec2e3968c9cbdf26a5e9c172be94f88af533e14bd43f892d5297"
    },
    {
      "path": "tools/generate",
      "mode": "executable",
      "size": 17,
      "digest": "sha256:306c6ca7407560340797866e077e053627ad409277d1b9da58106fce4cf717cb"
    },
    {
      "path": "vo.mod",
      "mode": "regular",
      "size": 46,
      "digest": "sha256:0c1be4dce8ab9b84cc8de853a161f518c28edce06923186197338be107db24f1"
    }
  ]
}
"#
        .as_bytes()
        .to_vec()
    }

    #[test]
    fn renders_the_complete_v1_golden_and_round_trips() {
        let manifest = sample();
        let bytes = manifest.render().unwrap();
        assert_eq!(bytes, sample_json());
        assert_eq!(TreeManifest::parse(&bytes).unwrap(), manifest);
        let json = String::from_utf8(bytes).unwrap();
        assert!(json.ends_with('\n'));
        assert!(json.contains("\"format\": 1"));
        assert!(json.contains("\"files\""));
        assert!(!json.contains("module_root"));
        assert!(!json.contains("artifacts"));
    }

    #[test]
    fn rejects_noncanonical_json_and_line_endings() {
        let canonical = sample().render().unwrap();
        assert!(TreeManifest::parse(&canonical[..canonical.len() - 1]).is_err());
        let crlf = String::from_utf8(canonical)
            .unwrap()
            .replace('\n', "\r\n")
            .into_bytes();
        assert!(TreeManifest::parse(&crlf).is_err());
    }

    #[test]
    fn rejects_unknown_fields_and_unsorted_files() {
        let unknown = br#"{"format":1,"files":[],"source":{}}"#;
        assert!(TreeManifest::parse(unknown).is_err());

        let mut manifest = sample();
        manifest.files.swap(0, 1);
        let error = manifest.render().unwrap_err().to_string();
        assert!(error.contains("sorted by path"), "{error}");
    }

    #[test]
    fn file_order_compares_raw_utf8_bytes_instead_of_utf16_code_units() {
        let bmp_path = "assets/\u{e000}.bin";
        let supplementary_path = "assets/\u{10000}.bin";
        assert!(bmp_path.as_bytes() < supplementary_path.as_bytes());
        assert!(bmp_path
            .encode_utf16()
            .cmp(supplementary_path.encode_utf16())
            .is_gt());

        let valid = TreeManifest {
            format: 1,
            files: vec![
                entry(bmp_path, b"bmp"),
                entry(supplementary_path, b"supplementary"),
                entry("vo.mod", b"x"),
            ],
        };
        valid.render().unwrap();

        let mut utf16_ordered = valid;
        utf16_ordered.files.swap(0, 1);
        let error = utf16_ordered.render().unwrap_err().to_string();
        assert!(error.contains("sorted by path"), "{error}");
    }

    #[test]
    fn requires_a_nonempty_root_mod_file() {
        let missing = TreeManifest {
            format: 1,
            files: vec![entry("src/main.vo", b"fn main() {}")],
        };
        assert!(missing.render().unwrap_err().to_string().contains("vo.mod"));

        let empty = TreeManifest {
            format: 1,
            files: vec![entry("vo.mod", b"")],
        };
        assert!(empty
            .render()
            .unwrap_err()
            .to_string()
            .contains("must not be empty"));

        let executable = TreeManifest {
            format: 1,
            files: vec![entry_with_mode("vo.mod", SourceFileMode::Executable, b"x")],
        };
        let error = executable.render().unwrap_err().to_string();
        assert!(error.contains("mode must be regular"), "{error}");
    }

    #[test]
    fn rejects_reserved_paths_and_portable_collisions() {
        let reserved = TreeManifest {
            format: 1,
            files: vec![entry("vo.mod", b"x"), entry("vo.tree.json", b"x")],
        };
        assert!(reserved
            .render()
            .unwrap_err()
            .to_string()
            .contains("reserved"));

        for nested_module in ["VO.MOD", "nested/vo.mod", "nested/VO.MOD", "Straße/VO.MOD"] {
            let malicious = TreeManifest {
                format: 1,
                files: vec![entry(nested_module, b"nested"), entry("vo.mod", b"root")],
            };
            let error = malicious.render().unwrap_err().to_string();
            assert!(error.contains("reserved"), "{nested_module}: {error}");
        }

        let collision = TreeManifest {
            format: 1,
            files: vec![
                entry("STRASSE.bin", b"x"),
                entry("Straße.bin", b"x"),
                entry("vo.mod", b"x"),
            ],
        };
        assert!(collision
            .render()
            .unwrap_err()
            .to_string()
            .contains("portable spelling"));
    }
}
