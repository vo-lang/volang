use crate::schema::{
    portable_case_key, validate_portable_path_component, MAX_PORTABLE_PATH_BYTES,
    MAX_PORTABLE_PATH_COMPONENTS,
};
use crate::{Error, MAX_MODULE_METADATA_ENTRIES};
use std::collections::BTreeSet;

/// Parsed representation of a `vo.work` file.
///
/// Workspace version 1 has one wire shape: a version and an array of member
/// directories. Module identities are deliberately absent from the workspace
/// file and are always read from each member's `vo.mod`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkFile {
    pub format: u64,
    pub members: Vec<String>,
}

impl WorkFile {
    /// Parse the strict version-1 `vo.work` TOML schema.
    pub fn parse(content: &str) -> Result<Self, Error> {
        if content.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::WorkFileParse(format!(
                "vo.work exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        let doc: toml::Value = content
            .parse()
            .map_err(|error| Error::WorkFileParse(format!("TOML parse error: {error}")))?;
        let table = doc
            .as_table()
            .ok_or_else(|| Error::WorkFileParse("expected TOML table at root".into()))?;
        reject_unknown_keys(table, &["format", "members"], "root")?;

        let format = table
            .get("format")
            .and_then(toml::Value::as_integer)
            .ok_or_else(|| Error::WorkFileParse("missing or non-integer 'format'".into()))?;
        let format = u64::try_from(format)
            .map_err(|_| Error::WorkFileParse("'format' must be a non-negative integer".into()))?;
        if format != 1 {
            return Err(Error::WorkFileParse(format!(
                "unsupported workspace file format: {format}"
            )));
        }

        let member_values = table
            .get("members")
            .and_then(toml::Value::as_array)
            .ok_or_else(|| Error::WorkFileParse("missing or non-array 'members'".into()))?;
        if member_values.is_empty() {
            return Err(Error::WorkFileParse(
                "'members' must contain at least one module".into(),
            ));
        }
        if member_values.len() > MAX_MODULE_METADATA_ENTRIES {
            return Err(Error::WorkFileParse(format!(
                "'members' contains {} entries, exceeding the {}-entry limit",
                member_values.len(),
                MAX_MODULE_METADATA_ENTRIES
            )));
        }
        let mut members = Vec::new();
        members
            .try_reserve(member_values.len())
            .map_err(|_| Error::WorkFileParse("cannot allocate workspace members".into()))?;
        let mut authored_paths = BTreeSet::new();
        let mut portable_paths = BTreeSet::new();
        for (index, value) in member_values.iter().enumerate() {
            let path = value.as_str().ok_or_else(|| {
                Error::WorkFileParse(format!("members[{index}]: expected string"))
            })?;
            validate_workspace_path(path)
                .map_err(|detail| Error::WorkFileParse(format!("members[{index}]: {detail}")))?;
            if !authored_paths.insert(path) {
                return Err(Error::WorkFileParse(format!(
                    "members[{index}]: duplicate member path {path:?}"
                )));
            }
            if !portable_paths.insert(portable_case_key(path)) {
                return Err(Error::WorkFileParse(format!(
                    "members[{index}]: member path {path:?} conflicts with another member under portable path spelling rules"
                )));
            }
            members.push(path.to_string());
        }

        Ok(Self { format, members })
    }

    /// Validate every invariant normally established by parsing `vo.work`.
    pub fn validate(&self) -> Result<(), Error> {
        if self.format != 1 {
            return Err(Error::WorkFileParse(format!(
                "unsupported workspace file format: {}",
                self.format
            )));
        }
        if self.members.is_empty() {
            return Err(Error::WorkFileParse(
                "'members' must contain at least one module".into(),
            ));
        }
        if self.members.len() > MAX_MODULE_METADATA_ENTRIES {
            return Err(Error::WorkFileParse(format!(
                "'members' contains more than {MAX_MODULE_METADATA_ENTRIES} entries"
            )));
        }
        let mut authored_paths = BTreeSet::new();
        let mut portable_paths = BTreeSet::new();
        for (index, path) in self.members.iter().enumerate() {
            validate_workspace_path(path)
                .map_err(|detail| Error::WorkFileParse(format!("members[{index}]: {detail}")))?;
            if !authored_paths.insert(path) {
                return Err(Error::WorkFileParse(format!(
                    "members[{index}]: duplicate member path {path:?}"
                )));
            }
            if !portable_paths.insert(portable_case_key(path)) {
                return Err(Error::WorkFileParse(format!(
                    "members[{index}]: member path {path:?} conflicts with another member under portable path spelling rules"
                )));
            }
        }
        Ok(())
    }

    /// Render the single canonical TOML spelling accepted for generated files.
    pub fn render(&self) -> Result<String, Error> {
        self.validate()?;
        let mut output = super::BoundedTextOutput::new(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(Error::WorkFileParse)?;
        output
            .push_str(&format!("format = {}\nmembers = [", self.format))
            .map_err(Error::WorkFileParse)?;
        let mut members = self.members.iter().collect::<Vec<_>>();
        members.sort();
        for (index, member) in members.into_iter().enumerate() {
            if index != 0 {
                output.push_str(", ").map_err(Error::WorkFileParse)?;
            }
            output
                .push_toml_string(member)
                .map_err(Error::WorkFileParse)?;
        }
        output.push_str("]\n").map_err(Error::WorkFileParse)?;
        let output = output.finish();
        Self::parse(&output)?;
        Ok(output)
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
    if path.len() > MAX_PORTABLE_PATH_BYTES {
        return Err("exceeds the portable path limit");
    }
    if vo_common::identifier::has_unicode_white_space_boundary(path) {
        return Err("must not contain leading or trailing whitespace");
    }
    if path.chars().any(vo_common::identifier::is_unicode_control) {
        return Err("must not contain control characters");
    }
    if path == "." {
        return Ok(());
    }
    if path.starts_with('/') || path.ends_with('/') || path.contains('\\') {
        return Err("must be a canonical slash-separated relative path");
    }
    let components = path.split('/').collect::<Vec<_>>();
    if components.len() > MAX_PORTABLE_PATH_COMPONENTS {
        return Err("contains too many path components");
    }
    for component in components {
        if component == ".." {
            return Err("parent components are not allowed");
        }
        validate_portable_path_component(component)
            .map_err(|_| "contains a non-portable path component")?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_members_only_wire_shape() {
        let workfile = WorkFile::parse(
            "format = 1\nmembers = [\".\", \"libraries/vogui\", \"tools/voplay\"]\n",
        )
        .unwrap();
        assert_eq!(workfile.format, 1);
        assert_eq!(workfile.members, [".", "libraries/vogui", "tools/voplay"]);
    }

    #[test]
    fn rejects_old_and_alternate_wire_shapes() {
        for source in [
            "version = 1\n",
            "format = 1\n[[use]]\npath = \"vogui\"\n",
            "format = 1\nmembers = [{ path = \"vogui\" }]\n",
            "format = 1\nmember = [\"vogui\"]\n",
            "format = 2\nmembers = [\".\"]\n",
            "format = 1\nmembers = []\n",
        ] {
            assert!(WorkFile::parse(source).is_err(), "accepted {source:?}");
        }
    }

    #[test]
    fn rejects_invalid_or_duplicate_member_paths() {
        for source in [
            "format = 1\nmembers = [\"\"]\n",
            "format = 1\nmembers = [\" vogui\"]\n",
            "format = 1\nmembers = [\"vogui\\n\"]\n",
            "format = 1\nmembers = [\"vogui\u{a0}\"]\n",
            "format = 1\nmembers = [\"vogui\u{85}\"]\n",
            "format = 1\nmembers = [\"/vogui\"]\n",
            "format = 1\nmembers = [\"./vogui\"]\n",
            "format = 1\nmembers = [\"vogui/../voplay\"]\n",
            "format = 1\nmembers = [\"../vogui\"]\n",
            "format = 1\nmembers = [\"vogui/\"]\n",
            "format = 1\nmembers = [\"vogui\\\\native\"]\n",
            "format = 1\nmembers = [\"vogui\", \"vogui\"]\n",
            "format = 1\nmembers = [\"VOGUI\", \"vogui\"]\n",
        ] {
            assert!(WorkFile::parse(source).is_err(), "accepted {source:?}");
        }
    }

    #[test]
    fn rejects_unknown_root_keys() {
        assert!(WorkFile::parse("format = 1\nmembers = [\".\"]\nfuture = true\n").is_err());
    }

    #[test]
    fn canonical_render_round_trips_portable_paths() {
        let workfile = WorkFile {
            format: 1,
            members: vec![".".into(), "目录/member".into()],
        };
        let rendered = workfile.render().unwrap();
        assert_eq!(rendered, "format = 1\nmembers = [\".\", \"目录/member\"]\n");
        assert_eq!(WorkFile::parse(&rendered).unwrap(), workfile);

        let invalid = WorkFile {
            format: 1,
            members: vec!["dir\u{8}name".into()],
        };
        assert!(invalid.render().is_err());
    }
}
