//! Inline `vo.mod` metadata parser (spec §5.6).
//!
//! A single `.vo` source file MAY embed `vo.mod` metadata in a reserved block
//! comment that starts with the sentinel `/*vo:mod` and appears before the
//! first non-whitespace, non-comment token of the file. Such a file becomes a
//! *single-file ephemeral module* (spec §10.2), identified by a reserved
//! `local/<name>` module path. Single-file modules are standard-library-only;
//! third-party dependencies require an ordinary project with `vo.mod` and
//! `vo.lock`.

use std::fmt;

use vo_common::span::Span;
use vo_syntax::ast::InlineModMetadata as SyntaxInlineModMetadata;

use crate::identity::ModIdentity;
use crate::schema::modfile::ModFile;
use crate::version::ToolchainConstraint;
use crate::Error;

/// Opening sentinel that introduces an inline mod block.
pub use vo_syntax::inline_mod::INLINE_MOD_OPEN;

/// Reserved prefix for any `vo:` directive block comment.
pub use vo_syntax::inline_mod::INLINE_MOD_RESERVED_PREFIX;

/// Closing delimiter for a block comment.
pub use vo_syntax::inline_mod::INLINE_MOD_CLOSE;

/// Parsed inline `vo.mod` metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlineMod {
    pub module: ModIdentity,
    pub version: crate::version::ExactVersion,
    pub vo: ToolchainConstraint,
}

/// Build the minimal compiler-owned manifest used to carry a single-file
/// module identity through project-shaped analysis APIs.
pub fn synthesize_mod_file(inline: &InlineMod) -> ModFile {
    ModFile {
        format: 1,
        module: inline.module.clone(),
        version: inline.version.clone(),
        vo: inline.vo.clone(),
        dependencies: Vec::new(),
        web: None,
        extension: None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlineModParseError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for InlineModParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for InlineModParseError {}

pub fn leading_reserved_block_span(src: &str, base: u32) -> Option<Span> {
    let (output, _) = vo_syntax::inline_mod::parse_leading_inline_mod(src, base);
    output.reserved_block.map(|block| block.span)
}

/// Attempt to extract an inline mod block from the beginning of a Vo source
/// file.
///
/// Returns:
///
/// - `Ok(None)` if the source does not begin with the `/*vo:mod` sentinel.
/// - `Ok(Some(inline_mod))` if a valid inline mod block is present.
/// - `Err(_)` if the block uses a reserved `/*vo:` prefix but is malformed
///   (wrong directive name, unterminated block, invalid body, etc.).
///
/// This function inspects only the file prefix; it does not classify tokens
/// that appear later in the source file.
pub fn parse_inline_mod_from_source(src: &str) -> Result<Option<InlineMod>, Error> {
    parse_inline_mod_from_source_with_span(src, 0)
        .map_err(|error| Error::ModFileParse(error.message))
}

pub fn parse_inline_mod_from_source_with_span(
    src: &str,
    base: u32,
) -> Result<Option<InlineMod>, InlineModParseError> {
    let (output, diagnostics) = vo_syntax::inline_mod::parse_leading_inline_mod(src, base);
    if let Some(diagnostic) = diagnostics.iter().next() {
        let span = diagnostic
            .labels
            .first()
            .map(|label| label.span)
            .unwrap_or_else(Span::dummy);
        return Err(InlineModParseError {
            message: diagnostic.message.clone(),
            span,
        });
    }

    match output.inline_mod {
        Some(inline_mod) => convert_inline_mod(inline_mod).map(Some),
        None => Ok(None),
    }
}

fn convert_inline_mod(
    inline_mod: SyntaxInlineModMetadata,
) -> Result<InlineMod, InlineModParseError> {
    let error_span = if inline_mod.body_span.is_empty() {
        inline_mod.span
    } else {
        inline_mod.body_span
    };
    let parsed = ModFile::parse_inline(&inline_mod.body).map_err(|error| {
        let detail = match error {
            Error::ModFileParse(detail) => detail,
            other => other.to_string(),
        };
        InlineModParseError {
            message: format!("invalid inline vo.mod: {detail}"),
            span: error_span,
        }
    })?;
    if !parsed.module.is_local() {
        return Err(InlineModParseError {
            message: "inline vo.mod module identity must use local/<name>".to_string(),
            span: error_span,
        });
    }

    Ok(InlineMod {
        module: parsed.module,
        version: parsed.version,
        vo: parsed.vo,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::identity::LocalName;

    #[test]
    fn no_inline_mod_when_no_sentinel() {
        let src = "package main\nfunc main() {}\n";
        assert!(parse_inline_mod_from_source(src).unwrap().is_none());
    }

    #[test]
    fn no_inline_mod_when_line_comments_only() {
        // Line-comment leading file is NOT an inline mod.
        let src = "// hello\npackage main\n";
        assert!(parse_inline_mod_from_source(src).unwrap().is_none());
    }

    #[test]
    fn no_inline_mod_when_block_comment_not_vo() {
        let src = "/* just a normal block comment */\npackage main\n";
        assert!(parse_inline_mod_from_source(src).unwrap().is_none());
    }

    #[test]
    fn reserved_vo_prefix_with_unknown_directive_is_error() {
        let src =
            "/*vo:meta\nformat = 1\nmodule = \"local/a\"\nversion = \"0.1.0\"\n*/\npackage main\n";
        let err = parse_inline_mod_from_source(src).unwrap_err();
        assert!(format!("{err}").contains("'/*vo:mod'"));
    }

    #[test]
    fn unterminated_inline_mod_is_error() {
        let src = "/*vo:mod\nformat = 1\nmodule = \"local/a\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\npackage main\n";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn parses_local_module_identity() {
        let src = "/*vo:mod\nformat = 1\nmodule = \"local/gui_chat\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\n";
        let inline = parse_inline_mod_from_source(src).unwrap().unwrap();
        assert!(inline.module.is_local());
        assert_eq!(inline.module.as_str(), "local/gui_chat");
    }

    #[test]
    fn parses_after_ordinary_leading_comments() {
        let src = "// license\n/* ordinary */\n/*vo:mod\nformat = 1\nmodule = \"local/gui_chat\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\n";
        let inline = parse_inline_mod_from_source(src).unwrap().unwrap();
        assert_eq!(inline.module.as_str(), "local/gui_chat");
    }

    #[test]
    fn rejects_publishable_module_identity() {
        let src = "/*vo:mod\nformat = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\n";
        let error = parse_inline_mod_from_source_with_span(src, 0).unwrap_err();
        assert!(error.message.contains("must use local/<name>"));
        assert!(src[error.span.to_range()].contains("github.com/acme/app"));
    }

    #[test]
    fn rejects_dependency_table() {
        let src = "\
/*vo:mod
module = \"local/gui_chat\"
vo = \"0.1.0\"

[dependencies]
\"github.com/vo-lang/vogui\" = \"^0.4.0\"
*/
package main
";
        let error = parse_inline_mod_from_source(src).unwrap_err();
        assert!(error.to_string().contains("unknown key 'dependencies'"));
    }

    #[test]
    fn dependency_error_carries_inline_body_span() {
        let src = "\
/*vo:mod
module = \"local/app\"
vo = \"0.1.0\"
[dependencies]
\"github.com/acme/lib\" = \"^0.1.0\"
*/
package main
";
        let err = parse_inline_mod_from_source_with_span(src, 0).unwrap_err();
        assert!(
            err.message.contains("unknown key 'dependencies'"),
            "{}",
            err.message
        );
        assert!(src[err.span.to_range()].contains("[dependencies]"));
    }

    #[test]
    fn rejects_project_only_root_sections() {
        for section in [
            "[dependencies]\n\"github.com/acme/lib\" = \"^1.0.0\"\n",
            "[publish]\ninclude = []\n",
            "[web]\nentry = \"main.vo\"\n",
            "[extension]\nname = \"demo\"\n",
            "[build]\n",
        ] {
            let src = format!(
                "/*vo:mod\nformat = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n{section}*/\npackage main\n"
            );
            let error = parse_inline_mod_from_source(&src).unwrap_err();
            assert!(
                error.to_string().contains("unknown key"),
                "{section:?}: {error}"
            );
        }
    }

    #[test]
    fn rejects_unknown_toml_root_key() {
        let src = "\
/*vo:mod
module = \"local/app\"
vo = \"0.1.0\"
files = []
*/
package main
";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn rejects_duplicate_module() {
        let src = "\
/*vo:mod
module = \"local/app\"
module = \"local/other\"
vo = \"0.1.0\"
*/
package main
";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn rejects_second_inline_mod_block() {
        let src = "\
/*vo:mod
module = \"local/a\"
vo = \"0.1.0\"
*/
/*vo:mod
module = \"local/b\"
vo = \"0.1.0\"
*/
package main
";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn rejects_empty_body() {
        let src = "/*vo:mod*/\npackage main\n";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn leading_whitespace_before_sentinel_is_allowed() {
        let src = "\n   /*vo:mod\nformat = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\n";
        let inline = parse_inline_mod_from_source(src).unwrap().unwrap();
        assert_eq!(inline.module.as_str(), "local/app");
    }

    #[test]
    fn inline_toml_acceptance_matches_restricted_mod_file_parser() {
        let bodies = [
            "\r\nmodule = \"local/app\"\r\nvo = \"0.1.0\"\r\n",
            "\n# comment\nformat = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
            "\nmodule\u{a0}= \"local/app\"\nvo = \"0.1.0\"\n",
            "\nformat = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"not-a-version\"\n",
            "\nformat = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/acme/lib/v2\" = \"^1.0.0\"\n",
            "\nformat = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[publish]\ninclude = []\n",
        ];

        for body in bodies {
            let source = format!("{INLINE_MOD_OPEN}{body}{INLINE_MOD_CLOSE}\npackage main\n");
            let inline_ok = parse_inline_mod_from_source(&source).is_ok();
            let mod_file_ok = crate::schema::modfile::ModFile::parse_inline(body)
                .map(|mod_file| mod_file.module.is_local())
                .unwrap_or(false);
            assert_eq!(inline_ok, mod_file_ok, "grammar drift for body {body:?}");
        }
    }

    #[test]
    fn synthesized_manifest_is_minimal_and_canonical() {
        let inline = parse_inline_mod_from_source(
            "/*vo:mod\nformat = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\n",
        )
        .unwrap()
        .unwrap();
        let manifest = synthesize_mod_file(&inline);
        assert!(manifest.dependencies.is_empty());
        assert!(manifest.web.is_none());
        assert!(manifest.extension.is_none());
        assert_eq!(
            manifest.render().unwrap(),
            "format = 1\nmodule = \"local/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
        );
    }

    #[test]
    fn local_name_rules() {
        assert!(LocalName::parse("local/gui_chat").is_ok());
        assert!(LocalName::parse("local/a").is_ok());
        assert!(LocalName::parse("local/1abc").is_ok());
        assert!(LocalName::parse("local/abc-def.1").is_ok());
        assert!(LocalName::parse("local/").is_err());
        assert!(LocalName::parse("local").is_err());
        assert!(LocalName::parse("local/A").is_err());
        assert!(LocalName::parse("local/a/b").is_err());
        assert!(LocalName::parse("local/-abc").is_err());
        assert!(LocalName::parse("github.com/acme/app").is_err());
    }
}
