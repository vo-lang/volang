use crate::identity::ModulePath;
use crate::{Error, MAX_MODULE_METADATA_ENTRIES};
use std::collections::BTreeSet;

/// Parsed representation of a `vo.work` file (TOML format, version 1).
#[derive(Debug, Clone)]
pub struct WorkFile {
    pub version: u64,
    pub uses: Vec<UseEntry>,
}

#[derive(Debug, Clone)]
pub struct UseEntry {
    /// Canonical module path. If omitted in the file, it is resolved
    /// from `<path>/vo.mod` by the workspace layer.
    pub module: Option<ModulePath>,
    /// Local directory path (may be relative to the vo.work directory).
    pub path: String,
}

impl WorkFile {
    /// Parse a `vo.work` TOML string.
    pub fn parse(content: &str) -> Result<Self, Error> {
        if content.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::WorkFileParse(format!(
                "vo.work exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        let doc: toml::Value = content
            .parse()
            .map_err(|e| Error::WorkFileParse(format!("TOML parse error: {e}")))?;
        let table = doc
            .as_table()
            .ok_or_else(|| Error::WorkFileParse("expected TOML table at root".into()))?;
        reject_unknown_keys(table, &["version", "use"], "root")?;

        let version = table
            .get("version")
            .and_then(|v| v.as_integer())
            .ok_or_else(|| Error::WorkFileParse("missing or non-integer 'version'".into()))?;
        let version = u64::try_from(version)
            .map_err(|_| Error::WorkFileParse("'version' must be a non-negative integer".into()))?;
        if version != 1 {
            return Err(Error::WorkFileParse(format!(
                "unsupported workspace file version: {version}"
            )));
        }

        let mut uses = Vec::new();
        if let Some(use_val) = table.get("use") {
            let arr = use_val
                .as_array()
                .ok_or_else(|| Error::WorkFileParse("'use' must be an array of tables".into()))?;
            if arr.len() > MAX_MODULE_METADATA_ENTRIES {
                return Err(Error::WorkFileParse(format!(
                    "'use' contains {} entries, exceeding the {}-entry limit",
                    arr.len(),
                    MAX_MODULE_METADATA_ENTRIES
                )));
            }
            uses.try_reserve(arr.len()).map_err(|_| {
                Error::WorkFileParse("cannot allocate workspace use entries".into())
            })?;
            let mut explicit_modules = BTreeSet::new();
            for (i, entry) in arr.iter().enumerate() {
                let t = entry
                    .as_table()
                    .ok_or_else(|| Error::WorkFileParse(format!("use[{i}]: expected table")))?;
                reject_unknown_keys(t, &["module", "path"], &format!("use[{i}]"))?;
                let path = t
                    .get("path")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| Error::WorkFileParse(format!("use[{i}]: missing 'path'")))?;
                validate_workspace_path(path)
                    .map_err(|detail| Error::WorkFileParse(format!("use[{i}].path: {detail}")))?;
                let module = if let Some(m) = t.get("module") {
                    let s = m.as_str().ok_or_else(|| {
                        Error::WorkFileParse(format!("use[{i}].module: expected string"))
                    })?;
                    let module = ModulePath::parse(s)
                        .map_err(|e| Error::WorkFileParse(format!("use[{i}].module: {e}")))?;
                    if !explicit_modules.insert(module.as_str().to_string()) {
                        return Err(Error::WorkFileParse(format!(
                            "use[{i}].module: duplicate explicit override for {module}"
                        )));
                    }
                    Some(module)
                } else {
                    None
                };
                uses.push(UseEntry {
                    module,
                    path: path.to_string(),
                });
            }
        }

        Ok(WorkFile { version, uses })
    }

    /// Validate every invariant normally established by parsing `vo.work`.
    pub fn validate(&self) -> Result<(), Error> {
        if self.version != 1 {
            return Err(Error::WorkFileParse(format!(
                "unsupported workspace file version: {}",
                self.version
            )));
        }
        if self.uses.len() > MAX_MODULE_METADATA_ENTRIES {
            return Err(Error::WorkFileParse(format!(
                "'use' contains more than {MAX_MODULE_METADATA_ENTRIES} entries"
            )));
        }
        let mut explicit_modules = BTreeSet::new();
        for (index, entry) in self.uses.iter().enumerate() {
            validate_workspace_path(&entry.path)
                .map_err(|detail| Error::WorkFileParse(format!("use[{index}].path: {detail}")))?;
            if entry.path.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
                return Err(Error::WorkFileParse(format!(
                    "use[{index}].path exceeds the {}-byte text limit",
                    vo_common::vfs::MAX_TEXT_FILE_BYTES
                )));
            }
            if let Some(module) = &entry.module {
                if !explicit_modules.insert(module) {
                    return Err(Error::WorkFileParse(format!(
                        "use[{index}].module: duplicate explicit override for {module}"
                    )));
                }
            }
        }
        Ok(())
    }

    /// Render canonical TOML within the parser's byte ceiling.
    pub fn render(&self) -> Result<String, Error> {
        self.validate()?;
        let mut out = super::BoundedTextOutput::new(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(Error::WorkFileParse)?;
        out.push_str(&format!("version = {}\n", self.version))
            .map_err(Error::WorkFileParse)?;
        for u in &self.uses {
            out.push_str("\n[[use]]\n").map_err(Error::WorkFileParse)?;
            if let Some(ref m) = u.module {
                out.push_str("module = ").map_err(Error::WorkFileParse)?;
                out.push_toml_string(m.as_str())
                    .map_err(Error::WorkFileParse)?;
                out.push_str("\n").map_err(Error::WorkFileParse)?;
            }
            out.push_str("path = ").map_err(Error::WorkFileParse)?;
            out.push_toml_string(&u.path)
                .map_err(Error::WorkFileParse)?;
            out.push_str("\n").map_err(Error::WorkFileParse)?;
        }
        let out = out.finish();
        Self::parse(&out)?;
        Ok(out)
    }
}

fn reject_unknown_keys(
    table: &toml::map::Map<String, toml::Value>,
    allowed: &[&str],
    scope: &str,
) -> Result<(), Error> {
    for key in table.keys() {
        if !allowed.contains(&key.as_str()) {
            return Err(Error::WorkFileParse(format!(
                "{scope}: unknown key '{key}'"
            )));
        }
    }
    Ok(())
}

fn validate_workspace_path(path: &str) -> Result<(), &'static str> {
    if path.is_empty() {
        return Err("must be non-empty");
    }
    if vo_common::identifier::has_unicode_white_space_boundary(path) {
        return Err("must not contain leading or trailing whitespace");
    }
    if path.chars().any(vo_common::identifier::is_unicode_control) {
        return Err("must not contain control characters");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic() {
        let content = r#"
version = 1

[[use]]
path = "../vogui"

[[use]]
module = "github.com/vo-lang/voplay"
path = "../voplay"
"#;
        let wf = WorkFile::parse(content).unwrap();
        assert_eq!(wf.version, 1);
        assert_eq!(wf.uses.len(), 2);
        assert!(wf.uses[0].module.is_none());
        assert_eq!(wf.uses[0].path, "../vogui");
        assert_eq!(
            wf.uses[1].module.as_ref().unwrap().as_str(),
            "github.com/vo-lang/voplay"
        );
    }

    #[test]
    fn test_reject_bad_version() {
        let content = "version = 2\n";
        assert!(WorkFile::parse(content).is_err());
    }

    #[test]
    fn test_roundtrip() {
        let content = r#"version = 1

[[use]]
module = "github.com/vo-lang/vogui"
path = "../vogui"

[[use]]
module = "github.com/vo-lang/voplay"
path = "../voplay"
"#;
        let wf = WorkFile::parse(content).unwrap();
        let rendered = wf.render().unwrap();
        let wf2 = WorkFile::parse(&rendered).unwrap();
        assert_eq!(wf2.uses.len(), 2);
    }

    #[test]
    fn rejects_unknown_root_and_use_keys() {
        assert!(WorkFile::parse("version = 1\nfuture = true\n").is_err());
        assert!(
            WorkFile::parse("version = 1\n[[use]]\npath = \"../lib\"\nfuture = true\n").is_err()
        );
    }

    #[test]
    fn rejects_invalid_paths_and_duplicate_explicit_modules() {
        assert!(WorkFile::parse("version = 1\n[[use]]\npath = \"\"\n").is_err());
        assert!(WorkFile::parse("version = 1\n[[use]]\npath = \" ../lib\"\n").is_err());
        assert!(WorkFile::parse("version = 1\n[[use]]\npath = \"../lib\u{a0}\"\n").is_err());
        assert!(WorkFile::parse("version = 1\n[[use]]\npath = \"../lib\u{85}\"\n").is_err());
        assert!(WorkFile::parse(
            "version = 1\n\
             [[use]]\nmodule = \"github.com/acme/lib\"\npath = \"../one\"\n\
             [[use]]\nmodule = \"github.com/acme/lib\"\npath = \"../two\"\n"
        )
        .is_err());
    }

    #[test]
    fn render_uses_toml_escaping() {
        let wf = WorkFile {
            version: 1,
            uses: vec![UseEntry {
                module: None,
                path: "dir\"name".to_string(),
            }],
        };
        let rendered = wf.render().unwrap();
        let parsed = WorkFile::parse(&rendered).unwrap();
        assert_eq!(parsed.uses[0].path, "dir\"name");

        let invalid = WorkFile {
            version: 1,
            uses: vec![UseEntry {
                module: None,
                path: "dir\u{8}name".to_string(),
            }],
        };
        assert!(invalid.render().is_err());
    }
}
