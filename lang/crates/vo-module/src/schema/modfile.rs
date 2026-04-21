use crate::identity::{ModIdentity, ModulePath};
use crate::version::{DepConstraint, ToolchainConstraint};
use crate::Error;

/// Parsed representation of a `vo.mod` file.
///
/// The root `module` directive MAY be either a canonical github path (for
/// published / on-disk projects) or a reserved `local/<name>` identity (for
/// toolchain-synthesized ephemeral single-file modules, spec §5.6.2). The
/// dispatch between the two is handled by `ModIdentity`; downstream
/// operations that require a github path (e.g. release staging, registry
/// fetches) MUST call `ModIdentity::as_github()` and error on the `Local`
/// variant.
#[derive(Debug, Clone)]
pub struct ModFile {
    pub module: ModIdentity,
    pub vo: ToolchainConstraint,
    pub require: Vec<Require>,
    pub replace: Vec<Replace>,
}

#[derive(Debug, Clone)]
pub struct Require {
    pub module: ModulePath,
    pub constraint: DepConstraint,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Replace {
    pub module: ModulePath,
    pub path: String,
}

impl ModFile {
    /// Parse a `vo.mod` file from its text content.
    pub fn parse(content: &str) -> Result<Self, Error> {
        let mut module: Option<ModIdentity> = None;
        let mut vo: Option<ToolchainConstraint> = None;
        let mut require: Vec<Require> = Vec::new();
        let mut replace: Vec<Replace> = Vec::new();

        for (line_num, raw_line) in content.lines().enumerate() {
            let line = raw_line.trim();
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            let parts: Vec<&str> = line.split_whitespace().collect();
            let directive = parts[0];
            match directive {
                "module" => {
                    if module.is_some() {
                        return Err(Error::ModFileParse(format!(
                            "line {}: duplicate 'module' directive",
                            line_num + 1
                        )));
                    }
                    if parts.len() < 2 {
                        return Err(Error::ModFileParse(format!(
                            "line {}: 'module' requires a path argument",
                            line_num + 1
                        )));
                    }
                    if parts.len() > 2 {
                        return Err(Error::ModFileParse(format!(
                            "line {}: unexpected tokens after module path",
                            line_num + 1
                        )));
                    }
                    module =
                        Some(ModIdentity::parse(parts[1]).map_err(|e| {
                            Error::ModFileParse(format!("line {}: {e}", line_num + 1))
                        })?);
                }
                "vo" => {
                    if vo.is_some() {
                        return Err(Error::ModFileParse(format!(
                            "line {}: duplicate 'vo' directive",
                            line_num + 1
                        )));
                    }
                    if parts.len() < 2 {
                        return Err(Error::ModFileParse(format!(
                            "line {}: 'vo' requires a constraint argument",
                            line_num + 1
                        )));
                    }
                    if parts.len() > 2 {
                        return Err(Error::ModFileParse(format!(
                            "line {}: unexpected tokens after vo constraint",
                            line_num + 1
                        )));
                    }
                    vo =
                        Some(ToolchainConstraint::parse(parts[1]).map_err(|e| {
                            Error::ModFileParse(format!("line {}: {e}", line_num + 1))
                        })?);
                }
                "require" => {
                    if parts.len() < 3 {
                        return Err(Error::ModFileParse(format!(
                            "line {}: 'require' needs <module-path> <constraint>",
                            line_num + 1
                        )));
                    }
                    let mp = ModulePath::parse(parts[1])
                        .map_err(|e| Error::ModFileParse(format!("line {}: {e}", line_num + 1)))?;
                    let constraint = DepConstraint::parse(parts[2])
                        .map_err(|e| Error::ModFileParse(format!("line {}: {e}", line_num + 1)))?;
                    // Check for duplicate module path
                    if require.iter().any(|r| r.module == mp) {
                        return Err(Error::ModFileParse(format!(
                            "line {}: duplicate require for {}",
                            line_num + 1,
                            mp
                        )));
                    }
                    require.push(Require {
                        module: mp,
                        constraint,
                    });
                }
                "replace" => {
                    if parts.len() != 4 || parts[2] != "=>" {
                        return Err(Error::ModFileParse(format!(
                            "line {}: 'replace' needs <module-path> => <path>",
                            line_num + 1
                        )));
                    }
                    let mp = ModulePath::parse(parts[1])
                        .map_err(|e| Error::ModFileParse(format!("line {}: {e}", line_num + 1)))?;
                    if replace.iter().any(|r| r.module == mp) {
                        return Err(Error::ModFileParse(format!(
                            "line {}: duplicate replace for {}",
                            line_num + 1,
                            mp
                        )));
                    }
                    replace.push(Replace {
                        module: mp,
                        path: parts[3].to_string(),
                    });
                }
                _ => {
                    return Err(Error::ModFileParse(format!(
                        "line {}: unknown directive '{directive}'",
                        line_num + 1
                    )));
                }
            }
        }

        let module =
            module.ok_or_else(|| Error::ModFileParse("missing 'module' directive".to_string()))?;
        let vo = vo.ok_or_else(|| Error::ModFileParse("missing 'vo' directive".to_string()))?;

        Ok(ModFile {
            module,
            vo,
            require,
            replace,
        })
    }

    /// Render the canonical `vo.mod` text.
    /// Canonical format: module, then vo, then require lines sorted by module path.
    pub fn render(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("module {}\n", self.module));
        out.push_str(&format!("vo {}\n", self.vo));
        if !self.require.is_empty() {
            out.push('\n');
            let mut sorted: Vec<&Require> = self.require.iter().collect();
            sorted.sort_by(|a, b| a.module.cmp(&b.module));
            for req in sorted {
                out.push_str(&format!("require {} {}\n", req.module, req.constraint));
            }
        }
        if !self.replace.is_empty() {
            out.push('\n');
            let mut sorted: Vec<&Replace> = self.replace.iter().collect();
            sorted.sort_by(|a, b| a.module.cmp(&b.module));
            for replace in sorted {
                out.push_str(&format!("replace {} => {}\n", replace.module, replace.path));
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic() {
        let content = "\
module github.com/acme/app
vo ^1.0.0

require github.com/vo-lang/vogui ^0.4.0
require github.com/acme/http v1.3.1
";
        let mf = ModFile::parse(content).unwrap();
        assert_eq!(mf.module.as_str(), "github.com/acme/app");
        assert_eq!(mf.vo.to_string(), "^1.0.0");
        assert_eq!(mf.require.len(), 2);
        assert!(mf.replace.is_empty());
    }

    #[test]
    fn test_parse_with_comments() {
        let content = "\
// This is my module
module github.com/acme/app
vo ^1.0.0
// A dependency
require github.com/vo-lang/vogui ^0.4.0
";
        let mf = ModFile::parse(content).unwrap();
        assert_eq!(mf.require.len(), 1);
    }

    #[test]
    fn test_parse_replace() {
        let content = "\
module github.com/acme/app
vo ^1.0.0
replace github.com/vo-lang/vogui => ../vogui
";
        let mf = ModFile::parse(content).unwrap();
        assert_eq!(mf.replace.len(), 1);
        assert_eq!(mf.replace[0].module.as_str(), "github.com/vo-lang/vogui");
        assert_eq!(mf.replace[0].path, "../vogui");
    }

    #[test]
    fn test_reject_duplicate_module() {
        let content = "\
module github.com/acme/app
module github.com/acme/other
vo ^1.0.0
";
        assert!(ModFile::parse(content).is_err());
    }

    #[test]
    fn test_reject_duplicate_require() {
        let content = "\
module github.com/acme/app
vo ^1.0.0
require github.com/vo-lang/vogui ^0.4.0
require github.com/vo-lang/vogui ^0.5.0
";
        assert!(ModFile::parse(content).is_err());
    }

    #[test]
    fn test_reject_duplicate_replace() {
        let content = "\
module github.com/acme/app
vo ^1.0.0
replace github.com/vo-lang/vogui => ../vogui
replace github.com/vo-lang/vogui => ../vogui-local
";
        assert!(ModFile::parse(content).is_err());
    }

    #[test]
    fn test_reject_unknown_directive() {
        let content = "\
module github.com/acme/app
vo ^1.0.0
files (something)
";
        assert!(ModFile::parse(content).is_err());
    }

    #[test]
    fn test_render_canonical() {
        let content = "\
module github.com/acme/app
vo ^1.0.0

require github.com/vo-lang/voplay ~0.7.2
require github.com/acme/http v1.3.1
require github.com/vo-lang/vogui ^0.4.0

replace github.com/vo-lang/resvg => ../resvg
replace github.com/vo-lang/voplay => ../voplay
";
        let mf = ModFile::parse(content).unwrap();
        let rendered = mf.render();
        let reparsed = ModFile::parse(&rendered).unwrap();
        // Check sorted order
        assert_eq!(reparsed.require[0].module.as_str(), "github.com/acme/http");
        assert_eq!(
            reparsed.require[1].module.as_str(),
            "github.com/vo-lang/vogui"
        );
        assert_eq!(
            reparsed.require[2].module.as_str(),
            "github.com/vo-lang/voplay"
        );
        assert_eq!(
            reparsed.replace[0].module.as_str(),
            "github.com/vo-lang/resvg"
        );
        assert_eq!(
            reparsed.replace[1].module.as_str(),
            "github.com/vo-lang/voplay"
        );
    }

    #[test]
    fn test_roundtrip() {
        let content = "\
module github.com/acme/app
vo ^1.0.0

require github.com/acme/http v1.3.1
require github.com/vo-lang/vogui ^0.4.0
";
        let mf = ModFile::parse(content).unwrap();
        let rendered = mf.render();
        let mf2 = ModFile::parse(&rendered).unwrap();
        assert_eq!(mf2.render(), rendered);
    }

    #[test]
    fn test_missing_module() {
        let content = "vo ^1.0.0\n";
        assert!(ModFile::parse(content).is_err());
    }

    #[test]
    fn test_missing_vo() {
        let content = "module github.com/acme/app\n";
        assert!(ModFile::parse(content).is_err());
    }
}
