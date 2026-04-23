use crate::ext_manifest::{ExtensionManifest, ModMetadata, WebProjectManifest};
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
    pub web: Option<WebProjectManifest>,
    pub extension: Option<ExtensionManifest>,
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
        let replace: Vec<Replace> = Vec::new();

        for (line_num, raw_line) in content.lines().enumerate() {
            let line = raw_line.trim();
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            if line.starts_with('[') {
                break;
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
                    return Err(Error::ModFileParse(format!(
                        "line {}: replace directives are not supported in vo.mod; use vo.work for local development",
                        line_num + 1
                    )));
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
        let ModMetadata { web, extension } = crate::ext_manifest::parse_mod_metadata_content(
            content,
            std::path::Path::new("vo.mod"),
        )?;

        Ok(ModFile {
            module,
            vo,
            require,
            web,
            extension,
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
        if let Some(web) = &self.web {
            out.push('\n');
            out.push_str("[web]\n");
            if let Some(entry) = &web.entry {
                out.push_str(&format!("entry = \"{}\"\n", entry));
            }
            if !web.include.is_empty() {
                out.push_str(&format!(
                    "include = [{}]\n",
                    render_path_array(&web.include)
                ));
            }
        }
        if let Some(extension) = &self.extension {
            out.push('\n');
            out.push_str("[extension]\n");
            out.push_str(&format!("name = \"{}\"\n", extension.name));
            if !extension.include.is_empty() {
                out.push_str(&format!(
                    "include = [{}]\n",
                    render_path_array(&extension.include)
                ));
            }
            if let Some(wasm) = &extension.wasm {
                out.push('\n');
                out.push_str("[extension.wasm]\n");
                out.push_str(&format!(
                    "type = \"{}\"\n",
                    match wasm.kind {
                        crate::ext_manifest::WasmExtensionKind::Standalone => "standalone",
                        crate::ext_manifest::WasmExtensionKind::Bindgen => "bindgen",
                    }
                ));
                out.push_str(&format!("wasm = \"{}\"\n", wasm.wasm));
                if let Some(js_glue) = &wasm.js_glue {
                    out.push_str(&format!("js_glue = \"{}\"\n", js_glue));
                }
                if let Some(local_wasm) = &wasm.local_wasm {
                    out.push_str(&format!("local_wasm = \"{}\"\n", local_wasm));
                }
                if let Some(local_js_glue) = &wasm.local_js_glue {
                    out.push_str(&format!("local_js_glue = \"{}\"\n", local_js_glue));
                }
            }
            if let Some(web_runtime) = &extension.web {
                out.push('\n');
                out.push_str("[extension.web]\n");
                if let Some(entry) = &web_runtime.entry {
                    out.push_str(&format!("entry = \"{}\"\n", entry));
                }
                if !web_runtime.capabilities.is_empty() {
                    out.push_str(&format!(
                        "capabilities = [{}]\n",
                        render_string_array(&web_runtime.capabilities)
                    ));
                }
                if !web_runtime.js_modules.is_empty() {
                    out.push('\n');
                    out.push_str("[extension.web.js]\n");
                    for (name, path) in &web_runtime.js_modules {
                        out.push_str(&format!("{} = \"{}\"\n", name, path));
                    }
                }
            }
            if let Some(native) = &extension.native {
                out.push('\n');
                out.push_str("[extension.native]\n");
                if let Some(path) = &native.path {
                    out.push_str(&format!("path = \"{}\"\n", path));
                }
                for target in &native.targets {
                    out.push('\n');
                    out.push_str("[[extension.native.targets]]\n");
                    out.push_str(&format!("target = \"{}\"\n", target.target));
                    out.push_str(&format!("library = \"{}\"\n", target.library));
                }
            }
        }
        out
    }
}

fn render_path_array(paths: &[std::path::PathBuf]) -> String {
    paths
        .iter()
        .map(|path| format!("\"{}\"", path.to_string_lossy()))
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_string_array(items: &[String]) -> String {
    items
        .iter()
        .map(|item| format!("\"{}\"", item))
        .collect::<Vec<_>>()
        .join(", ")
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
    fn test_reject_replace() {
        let content = "\
module github.com/acme/app
vo ^1.0.0
replace github.com/vo-lang/vogui => ../vogui
";
        assert!(ModFile::parse(content).is_err());
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
