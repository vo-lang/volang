use vo_common::diagnostics::{DiagnosticSink, Label};
use vo_common::span::Span;

use crate::ast::InlineModMetadata;
use crate::errors::SyntaxError;
use crate::lexer::source_position_error;

pub const INLINE_MOD_OPEN: &str = "/*vo:mod";
pub const INLINE_MOD_RESERVED_PREFIX: &str = "/*vo:";
pub const INLINE_MOD_CLOSE: &str = "*/";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LeadingReservedBlock {
    pub span: Span,
}

#[derive(Debug, Clone, Default)]
pub struct InlineModParseOutput {
    pub reserved_block: Option<LeadingReservedBlock>,
    pub inline_mod: Option<InlineModMetadata>,
}

pub fn parse_leading_inline_mod(source: &str, base: u32) -> (InlineModParseOutput, DiagnosticSink) {
    let mut output = InlineModParseOutput::default();
    let mut diagnostics = DiagnosticSink::new();

    if let Some(error) = source_position_error(source, base) {
        diagnostics.emit(error.diagnostic(base));
        return (output, diagnostics);
    }

    // Walk the complete leading comment region so the protocol's "before the
    // first source token" rule has one literal meaning. Unicode white space
    // remains an ordinary source character, matching the lexer.
    let Some(prefix_len) = leading_reserved_prefix_offset(source) else {
        return (output, diagnostics);
    };
    let trimmed = &source[prefix_len..];

    let block_end = trimmed
        .find(INLINE_MOD_CLOSE)
        .map(|idx| idx + INLINE_MOD_CLOSE.len())
        .unwrap_or(trimmed.len());
    let block_span = span(base, prefix_len, prefix_len + block_end);
    output.reserved_block = Some(LeadingReservedBlock { span: block_span });

    if !trimmed.starts_with(INLINE_MOD_OPEN) {
        diagnostics.emit(SyntaxError::InvalidInlineMod.at_with_message(
            span(base, prefix_len, prefix_len + directive_head_len(trimmed)),
            format!(
                "reserved sentinel '/*vo:' at start of file must be '{}'",
                INLINE_MOD_OPEN
            ),
        ));
        return (output, diagnostics);
    }

    let after_sentinel = &trimmed[INLINE_MOD_OPEN.len()..];
    let next_char = after_sentinel.chars().next();
    let follows_ok = matches!(next_char, Some(c) if is_source_whitespace(c))
        || after_sentinel.starts_with(INLINE_MOD_CLOSE);
    if !follows_ok {
        diagnostics.emit(SyntaxError::InvalidInlineMod.at_with_message(
            span(base, prefix_len, prefix_len + INLINE_MOD_OPEN.len()),
            format!(
                "'{}' must be followed by whitespace, newline, or '{}'",
                INLINE_MOD_OPEN, INLINE_MOD_CLOSE
            ),
        ));
        return (output, diagnostics);
    }

    let Some(close_idx) = after_sentinel.find(INLINE_MOD_CLOSE) else {
        diagnostics.emit(SyntaxError::InvalidInlineMod.at_with_message(
            block_span,
            format!(
                "unterminated '{}' block; missing closing '{}'",
                INLINE_MOD_OPEN, INLINE_MOD_CLOSE
            ),
        ));
        return (output, diagnostics);
    };

    let body = &after_sentinel[..close_idx];
    let after_close = &after_sentinel[close_idx + INLINE_MOD_CLOSE.len()..];
    if let Some(second_idx) = leading_reserved_prefix_offset(after_close) {
        let second_start =
            prefix_len + INLINE_MOD_OPEN.len() + close_idx + INLINE_MOD_CLOSE.len() + second_idx;
        diagnostics.emit(
            SyntaxError::InvalidInlineMod
                .at_with_message(
                    span(base, second_start, second_start + INLINE_MOD_OPEN.len()),
                    format!(
                        "only a single '{}' block is permitted per file",
                        INLINE_MOD_OPEN
                    ),
                )
                .with_label(Label::secondary(block_span).with_message("first inline mod block")),
        );
        return (output, diagnostics);
    }

    let body_start = prefix_len + INLINE_MOD_OPEN.len();
    let body_span = span(base, body_start, body_start + close_idx);
    output.inline_mod = Some(InlineModMetadata {
        span: block_span,
        body: body.to_string(),
        body_span,
    });

    (output, diagnostics)
}

/// Locate a reserved directive comment in the whitespace/comment prefix that
/// precedes the first source token. Ordinary leading comments are skipped
/// with the same nested-block shape accepted by the lexer. An unterminated
/// ordinary comment is left to the lexer diagnostic path.
fn leading_reserved_prefix_offset(source: &str) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut cursor = 0usize;
    loop {
        while cursor < bytes.len() && is_source_whitespace(char::from(bytes[cursor])) {
            cursor += 1;
        }
        let rest = &bytes[cursor..];
        if rest.starts_with(INLINE_MOD_RESERVED_PREFIX.as_bytes()) {
            return Some(cursor);
        }
        if rest.starts_with(b"//") {
            cursor += 2;
            while cursor < bytes.len() && bytes[cursor] != b'\n' {
                cursor += 1;
            }
            continue;
        }
        if rest.starts_with(b"/*") {
            let mut depth = 1usize;
            cursor += 2;
            while cursor < bytes.len() {
                if bytes[cursor..].starts_with(b"/*") {
                    depth = depth.checked_add(1)?;
                    cursor += 2;
                } else if bytes[cursor..].starts_with(b"*/") {
                    depth -= 1;
                    cursor += 2;
                    if depth == 0 {
                        break;
                    }
                } else {
                    cursor += 1;
                }
            }
            if depth != 0 {
                return None;
            }
            continue;
        }
        return None;
    }
}

fn directive_head_len(trimmed: &str) -> usize {
    for (idx, ch) in trimmed.char_indices() {
        if is_source_whitespace(ch) {
            return idx;
        }
        if trimmed[idx..].starts_with(INLINE_MOD_CLOSE) {
            return idx + INLINE_MOD_CLOSE.len();
        }
    }
    trimmed.len()
}

#[inline]
fn is_source_whitespace(character: char) -> bool {
    matches!(character, ' ' | '\t' | '\r' | '\n')
}

fn span(base: u32, start: usize, end: usize) -> Span {
    Span::from_u32(base + start as u32, base + end as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_inline_mod_when_no_reserved_prefix() {
        let (output, diagnostics) = parse_leading_inline_mod("package main\n", 0);
        assert!(output.reserved_block.is_none());
        assert!(output.inline_mod.is_none());
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn captures_original_toml_body_and_spans() {
        let body = "\nformat = 1\nmodule = \"local/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n\n[dependencies]\n\"github.com/acme/lib\" = \"^1.2.0\"\n";
        let source = format!("{INLINE_MOD_OPEN}{body}{INLINE_MOD_CLOSE}\npackage main\n");
        let (output, diagnostics) = parse_leading_inline_mod(&source, 0);
        assert!(diagnostics.is_empty());
        let inline_mod = output.inline_mod.expect("expected inline mod metadata");
        assert_eq!(inline_mod.body, body);
        assert_eq!(&source[inline_mod.body_span.to_range()], body);
        assert_eq!(
            &source[inline_mod.span.to_range()],
            format!("{INLINE_MOD_OPEN}{body}{INLINE_MOD_CLOSE}")
        );
    }

    #[test]
    fn accepts_inline_metadata_after_ordinary_leading_comments() {
        let source = "// license\n/* ordinary /* nested */ comment */\n/*vo:mod\nformat = 1\nmodule = \"local/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\n";
        let (output, diagnostics) = parse_leading_inline_mod(source, 0);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        assert_eq!(
            output.inline_mod.unwrap().body,
            "\nformat = 1\nmodule = \"local/demo\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"
        );
    }

    #[test]
    fn preserves_crlf_body_at_nonzero_base() {
        let source =
            "\r\n\t/*vo:mod\r\nmodule = \"local/demo\"\r\nvo = \"0.1.0\"\r\n*/\r\npackage main\r\n";
        let (output, diagnostics) = parse_leading_inline_mod(source, 41);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let inline_mod = output.inline_mod.expect("expected inline mod metadata");
        let local_range = (inline_mod.body_span.start.0 - 41) as usize
            ..(inline_mod.body_span.end.0 - 41) as usize;
        let captured = &source[local_range];
        assert_eq!(
            captured,
            "\r\nmodule = \"local/demo\"\r\nvo = \"0.1.0\"\r\n"
        );
        assert_eq!(inline_mod.body, captured);
    }

    #[test]
    fn unicode_white_space_never_changes_inline_token_boundaries() {
        for prefix in ['\u{00a0}', '\u{0085}'] {
            let source =
                format!("{prefix}/*vo:mod\nmodule local/demo\nvo ^0.1.0\n*/\npackage main\n");
            let (output, diagnostics) = parse_leading_inline_mod(&source, 0);
            assert!(output.reserved_block.is_none(), "{prefix:?}");
            assert!(output.inline_mod.is_none(), "{prefix:?}");
            assert!(diagnostics.is_empty(), "{prefix:?}: {diagnostics:?}");
        }

        for source in [
            "/*vo:mod\u{00a0}\nmodule local/demo\nvo ^0.1.0\n*/\npackage main\n",
            "/*vo:mod\u{0085}\nmodule local/demo\nvo ^0.1.0\n*/\npackage main\n",
        ] {
            let (output, diagnostics) = parse_leading_inline_mod(source, 0);
            assert!(output.inline_mod.is_none(), "{source:?}");
            assert!(diagnostics.has_errors(), "{source:?}");
        }

        for body in [
            "\nmodule\u{00a0}= \"local/demo\"\n",
            "\n\u{0085}module = \"local/demo\"\n",
        ] {
            let source = format!("{INLINE_MOD_OPEN}{body}{INLINE_MOD_CLOSE}\npackage main\n");
            let (output, diagnostics) = parse_leading_inline_mod(&source, 0);
            assert!(diagnostics.is_empty(), "{source:?}: {diagnostics:?}");
            assert_eq!(output.inline_mod.unwrap().body, body);
        }
    }

    #[test]
    fn rejects_reserved_non_mod_directive() {
        let (output, diagnostics) = parse_leading_inline_mod("/*vo:meta\n*/\npackage main\n", 0);
        assert!(output.reserved_block.is_some());
        assert!(output.inline_mod.is_none());
        assert!(diagnostics.has_errors());
        let diagnostic = diagnostics.iter().next().unwrap();
        assert!(diagnostic.message.contains("reserved sentinel"));
        assert_eq!(diagnostic.labels[0].span.start.0, 0);
    }

    #[test]
    fn leaves_invalid_or_empty_toml_for_the_module_layer() {
        for body in [
            "",
            "\nmodule = \"local/demo\"\nmodule = \"local/other\"\n",
            "\nthis is not TOML\n",
        ] {
            let source = format!("{INLINE_MOD_OPEN}{body}{INLINE_MOD_CLOSE}\npackage main\n");
            let (output, diagnostics) = parse_leading_inline_mod(&source, 0);
            assert!(diagnostics.is_empty(), "{body:?}: {diagnostics:?}");
            assert_eq!(output.inline_mod.unwrap().body, body);
        }
    }

    #[test]
    fn rejects_second_inline_mod_block() {
        let source = "/*vo:mod\nformat = 1\nmodule = \"local/a\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\n/*vo:mod\nformat = 1\nmodule = \"local/b\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n*/\npackage main\n";
        let (_, diagnostics) = parse_leading_inline_mod(source, 0);
        assert!(diagnostics.has_errors());
        let diagnostic = diagnostics.iter().next().unwrap();
        assert!(diagnostic.message.contains("only a single"));
        assert_eq!(diagnostic.labels.len(), 2);
    }

    #[test]
    fn sentinel_text_after_the_first_source_token_is_ordinary_source_content() {
        for source in [
            "package main\nvar message = \"/*vo:mod\"\n",
            "package main\n/*vo:mod this is an ordinary later comment */\n",
        ] {
            let (output, diagnostics) = parse_leading_inline_mod(source, 0);
            assert!(output.inline_mod.is_none(), "{source:?}");
            assert!(diagnostics.is_empty(), "{source:?}: {diagnostics:?}");
        }
    }
}
