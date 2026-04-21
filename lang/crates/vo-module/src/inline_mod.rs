//! Inline `vo.mod` metadata parser (spec §5.6).
//!
//! A single `.vo` source file MAY embed `vo.mod` metadata in a reserved block
//! comment that starts with the sentinel `/*vo:mod` and appears before the
//! first non-whitespace, non-comment token of the file. Such a file becomes a
//! *single-file ephemeral module* (spec §10.2), identified by a reserved
//! `local/<name>` module path that MUST NOT appear in `require` lines, in any
//! published registry metadata, or in any import statement.

use std::fmt;

use vo_common::span::Span;
use vo_syntax::ast::{InlineDirectiveValue, InlineModMetadata as SyntaxInlineModMetadata};

use crate::identity::{ModIdentity, ModulePath, LOCAL_NAMESPACE_PREFIX};
use crate::version::{DepConstraint, ToolchainConstraint};
use crate::Error;

/// Opening sentinel that introduces an inline mod block.
pub use vo_syntax::inline_mod::INLINE_MOD_OPEN;

/// Reserved prefix for any `vo:` directive block comment.
pub use vo_syntax::inline_mod::INLINE_MOD_RESERVED_PREFIX;

/// Closing delimiter for a block comment.
pub use vo_syntax::inline_mod::INLINE_MOD_CLOSE;

/// Parsed inline `vo.mod` metadata.
#[derive(Debug, Clone)]
pub struct InlineMod {
    pub module: ModIdentity,
    pub vo: ToolchainConstraint,
    pub require: Vec<InlineRequire>,
}

/// A `require` line inside an inline mod.
///
/// The module path MUST be a canonical github path; `local/*` paths are
/// rejected at parse time (spec §5.6.3, §3.5).
#[derive(Debug, Clone)]
pub struct InlineRequire {
    pub module: ModulePath,
    pub constraint: DepConstraint,
}

#[derive(Debug, Clone)]
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
    let module = parse_inline_module_identity(&inline_mod.module)?;
    let vo = parse_toolchain_constraint(&inline_mod.vo)?;
    let mut require = Vec::with_capacity(inline_mod.require.len());
    for entry in inline_mod.require {
        require.push(convert_inline_require(entry)?);
    }
    Ok(InlineMod {
        module,
        vo,
        require,
    })
}

fn convert_inline_require(
    require: vo_syntax::ast::InlineModRequire,
) -> Result<InlineRequire, InlineModParseError> {
    if require.module.value.starts_with(LOCAL_NAMESPACE_PREFIX) {
        return Err(InlineModParseError {
            message: "'local/*' paths are not allowed in require".to_string(),
            span: require.module.span,
        });
    }
    Ok(InlineRequire {
        module: parse_module_path(&require.module)?,
        constraint: parse_dep_constraint(&require.constraint)?,
    })
}

fn parse_inline_module_identity(
    value: &InlineDirectiveValue,
) -> Result<ModIdentity, InlineModParseError> {
    ModIdentity::parse(&value.value).map_err(|error| InlineModParseError {
        message: error.to_string(),
        span: value.span,
    })
}

fn parse_toolchain_constraint(
    value: &InlineDirectiveValue,
) -> Result<ToolchainConstraint, InlineModParseError> {
    ToolchainConstraint::parse(&value.value).map_err(|error| InlineModParseError {
        message: error.to_string(),
        span: value.span,
    })
}

fn parse_module_path(value: &InlineDirectiveValue) -> Result<ModulePath, InlineModParseError> {
    ModulePath::parse(&value.value).map_err(|error| InlineModParseError {
        message: error.to_string(),
        span: value.span,
    })
}

fn parse_dep_constraint(
    value: &InlineDirectiveValue,
) -> Result<DepConstraint, InlineModParseError> {
    DepConstraint::parse(&value.value).map_err(|error| InlineModParseError {
        message: error.to_string(),
        span: value.span,
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
        let src = "/*vo:meta\nmodule local/a\n*/\npackage main\n";
        let err = parse_inline_mod_from_source(src).unwrap_err();
        assert!(format!("{err}").contains("'/*vo:mod'"));
    }

    #[test]
    fn unterminated_inline_mod_is_error() {
        let src = "/*vo:mod\nmodule local/a\nvo ^0.1.0\npackage main\n";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn parses_local_module_identity() {
        let src = "/*vo:mod\nmodule local/gui_chat\nvo ^0.1.0\n*/\npackage main\n";
        let inline = parse_inline_mod_from_source(src).unwrap().unwrap();
        assert!(inline.module.is_local());
        assert_eq!(inline.module.as_str(), "local/gui_chat");
        assert_eq!(inline.require.len(), 0);
    }

    #[test]
    fn parses_github_module_identity() {
        let src = "/*vo:mod\nmodule github.com/acme/app\nvo ^0.1.0\n*/\npackage main\n";
        let inline = parse_inline_mod_from_source(src).unwrap().unwrap();
        assert!(!inline.module.is_local());
        assert_eq!(inline.module.as_str(), "github.com/acme/app");
    }

    #[test]
    fn parses_require_entries() {
        let src = "\
/*vo:mod
module local/gui_chat
vo ^0.1.0

require github.com/vo-lang/vogui ^0.4.0
*/
package main
";
        let inline = parse_inline_mod_from_source(src).unwrap().unwrap();
        assert_eq!(inline.require.len(), 1);
        assert_eq!(
            inline.require[0].module.as_str(),
            "github.com/vo-lang/vogui"
        );
    }

    #[test]
    fn rejects_local_require() {
        let src = "\
/*vo:mod
module local/app
vo ^0.1.0
require local/other ^0.1.0
*/
package main
";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn local_require_error_carries_value_span() {
        let src = "\
/*vo:mod
module local/app
vo ^0.1.0
require local/other ^0.1.0
*/
package main
";
        let err = parse_inline_mod_from_source_with_span(src, 0).unwrap_err();
        assert_eq!(err.message, "'local/*' paths are not allowed in require");
        assert_eq!(&src[err.span.to_range()], "local/other");
    }

    #[test]
    fn rejects_replace_directive() {
        let src = "\
/*vo:mod
module local/app
vo ^0.1.0
replace github.com/vo-lang/vogui => ../vogui
*/
package main
";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn rejects_unknown_directive() {
        let src = "\
/*vo:mod
module local/app
vo ^0.1.0
files (x)
*/
package main
";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn rejects_duplicate_require() {
        let src = "\
/*vo:mod
module local/app
vo ^0.1.0
require github.com/vo-lang/vogui ^0.4.0
require github.com/vo-lang/vogui ^0.5.0
*/
package main
";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn rejects_duplicate_module() {
        let src = "\
/*vo:mod
module local/app
module local/other
vo ^0.1.0
*/
package main
";
        assert!(parse_inline_mod_from_source(src).is_err());
    }

    #[test]
    fn rejects_second_inline_mod_block() {
        let src = "\
/*vo:mod
module local/a
vo ^0.1.0
*/
/*vo:mod
module local/b
vo ^0.1.0
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
        let src = "\n   /*vo:mod\nmodule local/app\nvo ^0.1.0\n*/\npackage main\n";
        let inline = parse_inline_mod_from_source(src).unwrap().unwrap();
        assert_eq!(inline.module.as_str(), "local/app");
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
