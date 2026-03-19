use crate::identity::ModulePath;
use crate::Error;

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
        let doc: toml::Value = content
            .parse()
            .map_err(|e| Error::WorkFileParse(format!("TOML parse error: {e}")))?;
        let table = doc
            .as_table()
            .ok_or_else(|| Error::WorkFileParse("expected TOML table at root".into()))?;

        let version = table
            .get("version")
            .and_then(|v| v.as_integer())
            .map(|v| v as u64)
            .ok_or_else(|| Error::WorkFileParse("missing or non-integer 'version'".into()))?;
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
            for (i, entry) in arr.iter().enumerate() {
                let t = entry.as_table().ok_or_else(|| {
                    Error::WorkFileParse(format!("use[{i}]: expected table"))
                })?;
                let path = t
                    .get("path")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        Error::WorkFileParse(format!("use[{i}]: missing 'path'"))
                    })?
                    .to_string();
                let module = if let Some(m) = t.get("module") {
                    let s = m.as_str().ok_or_else(|| {
                        Error::WorkFileParse(format!("use[{i}].module: expected string"))
                    })?;
                    Some(ModulePath::parse(s).map_err(|e| {
                        Error::WorkFileParse(format!("use[{i}].module: {e}"))
                    })?)
                } else {
                    None
                };
                uses.push(UseEntry { module, path });
            }
        }

        Ok(WorkFile { version, uses })
    }

    /// Render canonical TOML.
    pub fn render(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("version = {}\n", self.version));
        for u in &self.uses {
            out.push_str("\n[[use]]\n");
            if let Some(ref m) = u.module {
                out.push_str(&format!("module = {:?}\n", m.as_str()));
            }
            out.push_str(&format!("path = {:?}\n", u.path));
        }
        out
    }
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
        let rendered = wf.render();
        let wf2 = WorkFile::parse(&rendered).unwrap();
        assert_eq!(wf2.uses.len(), 2);
    }
}
