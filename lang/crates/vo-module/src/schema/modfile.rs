use crate::ext_manifest::{ExtensionManifest, ModMetadata, WebProjectManifest};
use crate::identity::{ModIdentity, ModulePath};
use crate::version::{DepConstraint, ToolchainConstraint};
use crate::Error;
use std::collections::BTreeSet;

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
        if content.len() > vo_common::vfs::MAX_TEXT_FILE_BYTES {
            return Err(Error::ModFileParse(format!(
                "vo.mod exceeds the {}-byte text limit",
                vo_common::vfs::MAX_TEXT_FILE_BYTES
            )));
        }
        let mut module: Option<ModIdentity> = None;
        let mut vo: Option<ToolchainConstraint> = None;
        let mut require: Vec<Require> = Vec::new();
        let mut required_modules = BTreeSet::new();
        let replace: Vec<Replace> = Vec::new();

        for (line_num, raw_line) in content.lines().enumerate() {
            // Directive grammar is intentionally ASCII: Unicode whitespace is
            // data in paths and constraints and must never be silently
            // discarded according to the Rust toolchain's Unicode tables.
            let line = raw_line.trim_matches([' ', '\t']);
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            if line.starts_with('[') {
                break;
            }
            let parts: Vec<&str> = line
                .split([' ', '\t'])
                .filter(|part| !part.is_empty())
                .collect();
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
                    if parts.len() > 3 {
                        return Err(Error::ModFileParse(format!(
                            "line {}: unexpected tokens after require constraint",
                            line_num + 1
                        )));
                    }
                    if require.len() >= crate::MAX_MODULE_DEPENDENCIES {
                        return Err(Error::ModFileParse(format!(
                            "vo.mod contains more than {} direct dependencies",
                            crate::MAX_MODULE_DEPENDENCIES
                        )));
                    }
                    let mp = ModulePath::parse(parts[1])
                        .map_err(|e| Error::ModFileParse(format!("line {}: {e}", line_num + 1)))?;
                    let constraint = DepConstraint::parse(parts[2])
                        .map_err(|e| Error::ModFileParse(format!("line {}: {e}", line_num + 1)))?;
                    let lower_bound =
                        crate::version::ExactVersion::from_semver(constraint.version.clone());
                    if !mp.accepts_version(&lower_bound) {
                        return Err(Error::ModFileParse(format!(
                            "line {}: constraint {} is incompatible with module path {}",
                            line_num + 1,
                            constraint,
                            mp
                        )));
                    }
                    // Check for duplicate module path
                    if !required_modules.insert(mp.clone()) {
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
        if let Some(module_path) = module.as_github() {
            if require
                .iter()
                .any(|dependency| dependency.module == *module_path)
            {
                return Err(Error::ModFileParse(format!(
                    "module {} must not require itself",
                    module_path
                )));
            }
        }
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

    /// Validate every invariant normally established by parsing `vo.mod`.
    pub fn validate(&self) -> Result<(), Error> {
        if self.require.len() > crate::MAX_MODULE_DEPENDENCIES {
            return Err(Error::ModFileParse(format!(
                "require contains more than {} modules",
                crate::MAX_MODULE_DEPENDENCIES
            )));
        }
        if !self.replace.is_empty() {
            return Err(Error::ModFileParse(
                "replace directives cannot be rendered in vo.mod; use vo.work".to_string(),
            ));
        }
        let mut required_modules = BTreeSet::new();
        for (index, requirement) in self.require.iter().enumerate() {
            if !required_modules.insert(&requirement.module) {
                return Err(Error::ModFileParse(format!(
                    "duplicate require for {}",
                    requirement.module
                )));
            }
            if self.module.as_github() == Some(&requirement.module) {
                return Err(Error::ModFileParse(format!(
                    "module {} must not require itself",
                    requirement.module
                )));
            }
            let lower_bound =
                crate::version::ExactVersion::from_semver(requirement.constraint.version.clone());
            if !requirement.module.accepts_version(&lower_bound) {
                return Err(Error::ModFileParse(format!(
                    "require[{index}] constraint {} is incompatible with module path {}",
                    requirement.constraint, requirement.module
                )));
            }
        }
        if let Some(web) = &self.web {
            validate_render_count(web.include.len(), "web.include")?;
            validate_web_project_manifest(web)?;
        }
        if let Some(extension) = &self.extension {
            extension.validate()?;
        }
        Ok(())
    }

    /// Render the canonical `vo.mod` text.
    /// Canonical format: module, then vo, then require lines sorted by module path.
    pub fn render(&self) -> Result<String, Error> {
        self.validate()?;
        let mut out = super::BoundedTextOutput::new(vo_common::vfs::MAX_TEXT_FILE_BYTES)
            .map_err(Error::ModFileParse)?;
        macro_rules! push {
            ($value:expr) => {
                out.push_str($value).map_err(Error::ModFileParse)?
            };
        }
        macro_rules! quoted {
            ($value:expr) => {
                out.push_toml_string($value).map_err(Error::ModFileParse)?
            };
        }

        push!(&format!("module {}\n", self.module));
        push!(&format!("vo {}\n", self.vo));
        if !self.require.is_empty() {
            push!("\n");
            let mut sorted: Vec<&Require> = self.require.iter().collect();
            sorted.sort_by(|a, b| a.module.cmp(&b.module));
            for req in sorted {
                push!(&format!("require {} {}\n", req.module, req.constraint));
            }
        }
        if let Some(web) = &self.web {
            push!("\n[web]\n");
            if let Some(entry) = &web.entry {
                push!("entry = ");
                quoted!(entry);
                push!("\n");
            }
            if !web.include.is_empty() {
                push!("include = [");
                append_path_array(&mut out, &web.include)?;
                push!("]\n");
            }
        }
        if let Some(extension) = &self.extension {
            push!("\n[extension]\nname = ");
            quoted!(&extension.name);
            push!("\n");
            if !extension.include.is_empty() {
                push!("include = [");
                append_path_array(&mut out, &extension.include)?;
                push!("]\n");
            }
            if let Some(wasm) = &extension.wasm {
                push!("\n[extension.wasm]\ntype = \"");
                push!(match wasm.kind {
                    crate::ext_manifest::WasmExtensionKind::Standalone => "standalone",
                    crate::ext_manifest::WasmExtensionKind::Bindgen => "bindgen",
                });
                push!("\"\nwasm = ");
                quoted!(&wasm.wasm);
                push!("\n");
                if let Some(js_glue) = &wasm.js_glue {
                    push!("js_glue = ");
                    quoted!(js_glue);
                    push!("\n");
                }
                if let Some(local_wasm) = &wasm.local_wasm {
                    push!("local_wasm = ");
                    quoted!(local_wasm);
                    push!("\n");
                }
                if let Some(local_js_glue) = &wasm.local_js_glue {
                    push!("local_js_glue = ");
                    quoted!(local_js_glue);
                    push!("\n");
                }
            }
            if let Some(web_runtime) = &extension.web {
                push!("\n[extension.web]\n");
                if let Some(entry) = &web_runtime.entry {
                    push!("entry = ");
                    quoted!(entry);
                    push!("\n");
                }
                if !web_runtime.capabilities.is_empty() {
                    push!("capabilities = [");
                    append_string_array(&mut out, &web_runtime.capabilities)?;
                    push!("]\n");
                }
                if !web_runtime.js_modules.is_empty() {
                    push!("\n[extension.web.js]\n");
                    for (name, path) in &web_runtime.js_modules {
                        push!(name);
                        push!(" = ");
                        quoted!(path);
                        push!("\n");
                    }
                }
            }
            if let Some(native) = &extension.native {
                push!("\n[extension.native]\n");
                if let Some(path) = &native.path {
                    push!("path = ");
                    quoted!(path);
                    push!("\n");
                }
                let mut targets = native.targets.iter().collect::<Vec<_>>();
                targets.sort_by(|left, right| left.target.cmp(&right.target));
                for target in targets {
                    push!("\n[[extension.native.targets]]\ntarget = ");
                    quoted!(&target.target);
                    push!("\nlibrary = ");
                    quoted!(&target.library);
                    push!("\n");
                }
            }
        }
        let out = out.finish();
        Self::parse(&out)?;
        Ok(out)
    }
}

fn validate_render_count(len: usize, field: &str) -> Result<(), Error> {
    if len > crate::MAX_MODULE_METADATA_ENTRIES {
        return Err(Error::ModFileParse(format!(
            "{field} contains more than {} entries",
            crate::MAX_MODULE_METADATA_ENTRIES
        )));
    }
    Ok(())
}

fn validate_web_project_manifest(web: &WebProjectManifest) -> Result<(), Error> {
    if let Some(entry) = web.entry.as_deref() {
        if entry.is_empty()
            || entry.len() > crate::schema::MAX_PORTABLE_PATH_BYTES
            || vo_common::identifier::has_unicode_white_space_boundary(entry)
            || entry.chars().any(vo_common::identifier::is_unicode_control)
        {
            return Err(Error::ModFileParse(
                "[web].entry must be a non-empty normalized metadata string".to_string(),
            ));
        }
    }
    let mut paths = crate::schema::PortablePathSet::default();
    for (index, path) in web.include.iter().enumerate() {
        let portable = crate::schema::portable_relative_path_from_path(path)
            .map_err(|detail| Error::ModFileParse(format!("[web].include[{index}]: {detail}")))?;
        if !paths
            .insert_path(&portable)
            .map_err(|detail| Error::ModFileParse(format!("[web].include[{index}]: {detail}")))?
        {
            return Err(Error::ModFileParse(format!(
                "duplicate path in [web].include: {portable}"
            )));
        }
    }
    Ok(())
}

fn append_path_array(
    output: &mut super::BoundedTextOutput,
    input: &[std::path::PathBuf],
) -> Result<(), Error> {
    let mut paths = Vec::new();
    paths.try_reserve(input.len()).map_err(|_| {
        Error::ModFileParse("failed to reserve metadata paths for rendering".to_string())
    })?;
    for path in input {
        paths.push(path.to_str().ok_or_else(|| {
            Error::ModFileParse("metadata paths must be valid UTF-8".to_string())
        })?);
    }
    paths.sort();
    for (index, path) in paths.into_iter().enumerate() {
        if index != 0 {
            output.push_str(", ").map_err(Error::ModFileParse)?;
        }
        output.push_toml_string(path).map_err(Error::ModFileParse)?;
    }
    Ok(())
}

fn append_string_array(
    output: &mut super::BoundedTextOutput,
    items: &[String],
) -> Result<(), Error> {
    let mut sorted = Vec::new();
    sorted.try_reserve(items.len()).map_err(|_| {
        Error::ModFileParse("failed to reserve metadata strings for rendering".to_string())
    })?;
    sorted.extend(items);
    let mut items = sorted;
    items.sort();
    for (index, item) in items.into_iter().enumerate() {
        if index != 0 {
            output.push_str(", ").map_err(Error::ModFileParse)?;
        }
        output.push_toml_string(item).map_err(Error::ModFileParse)?;
    }
    Ok(())
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
    fn directive_grammar_uses_only_ascii_horizontal_space() {
        let parsed =
            ModFile::parse("\tmodule\tgithub.com/acme/app\t\r\n\tvo\t^1.0.0\t\r\n").unwrap();
        assert_eq!(parsed.module.as_str(), "github.com/acme/app");

        for content in [
            "module\u{a0}github.com/acme/app\nvo ^1.0.0\n",
            "module github.com/acme/app\u{a0}\nvo ^1.0.0\n",
            "\u{a0}module github.com/acme/app\nvo ^1.0.0\n",
            "module\u{85}github.com/acme/app\nvo ^1.0.0\n",
        ] {
            assert!(ModFile::parse(content).is_err(), "{content:?}");
        }
    }

    #[test]
    fn crlf_metadata_section_starts_at_the_exact_byte_boundary() {
        let parsed = ModFile::parse(
            "module github.com/acme/app\r\nvo ^1.0.0\r\n\r\n[extension]\r\nname = \"demo\"\r\n",
        )
        .unwrap();
        assert_eq!(parsed.extension.unwrap().name, "demo");
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
    fn test_reject_trailing_require_tokens() {
        let content = "\
module github.com/acme/app
vo ^1.0.0
require github.com/vo-lang/vogui ^0.4.0 trailing
";
        let error = ModFile::parse(content).unwrap_err().to_string();
        assert!(error.contains("unexpected tokens after require constraint"));
    }

    #[test]
    fn test_reject_self_requirement() {
        let content = "\
module github.com/acme/app
vo ^1.0.0
require github.com/acme/app ^1.0.0
";
        let error = ModFile::parse(content).unwrap_err().to_string();
        assert!(error.contains("must not require itself"));
    }

    #[test]
    fn test_reject_constraint_incompatible_with_module_major_suffix() {
        let content = "\
module github.com/acme/app
vo ^1.0.0
require github.com/acme/library/v2 ^1.4.0
";
        let error = ModFile::parse(content).unwrap_err().to_string();
        assert!(error.contains("is incompatible with module path"));
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
        let rendered = mf.render().unwrap();
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
        let rendered = mf.render().unwrap();
        let mf2 = ModFile::parse(&rendered).unwrap();
        assert_eq!(mf2.render().unwrap(), rendered);
    }

    #[test]
    fn render_rejects_invalid_public_values() {
        let mut file = ModFile::parse(
            "module github.com/acme/app\nvo ^1.0.0\nrequire github.com/acme/lib ^1.0.0\n",
        )
        .unwrap();
        file.require.push(file.require[0].clone());
        assert!(file.render().is_err());
    }

    #[test]
    fn test_render_roundtrips_portable_metadata_strings() {
        let content = r#"module github.com/acme/app
vo ^1.0.0

[extension]
name = "demo"

[extension.web]
capabilities = ["clipboard-read"]

[extension.web.js]
renderer = "js/renderer'quoted.js"
"#;
        let parsed = ModFile::parse(content).unwrap();
        let rendered = parsed.render().unwrap();
        let reparsed = ModFile::parse(&rendered).unwrap();

        assert_eq!(reparsed.extension, parsed.extension);
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
