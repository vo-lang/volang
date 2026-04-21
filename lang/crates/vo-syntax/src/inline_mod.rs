use vo_common::diagnostics::{Diagnostic, DiagnosticSink, Label};
use vo_common::span::Span;

use crate::ast::{InlineDirectiveValue, InlineModMetadata, InlineModRequire};
use crate::errors::SyntaxError;

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

    let trimmed = source.trim_start_matches(|c: char| c.is_whitespace());
    let prefix_len = source.len().saturating_sub(trimmed.len());

    if !trimmed.starts_with(INLINE_MOD_RESERVED_PREFIX) {
        return (output, diagnostics);
    }

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
    let follows_ok = matches!(next_char, Some(c) if c.is_whitespace())
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
    if let Some(second_idx) = after_close.find(INLINE_MOD_OPEN) {
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
    match parse_inline_body(body, base, body_start, body_span, block_span) {
        Ok(inline_mod) => output.inline_mod = Some(inline_mod),
        Err(diagnostic) => diagnostics.emit(diagnostic),
    }

    (output, diagnostics)
}

fn parse_inline_body(
    body: &str,
    base: u32,
    body_start: usize,
    body_span: Span,
    block_span: Span,
) -> Result<InlineModMetadata, Diagnostic> {
    let mut module: Option<InlineDirectiveValue> = None;
    let mut vo: Option<InlineDirectiveValue> = None;
    let mut require = Vec::new();
    let mut line_offset = 0usize;

    for raw_line in body.split_inclusive('\n') {
        let Some((trimmed_start, trimmed_end)) = trim_bounds(raw_line) else {
            line_offset += raw_line.len();
            continue;
        };
        let trimmed = &raw_line[trimmed_start..trimmed_end];
        if trimmed.starts_with("//") {
            line_offset += raw_line.len();
            continue;
        }

        let line_start = body_start + line_offset + trimmed_start;
        let line_end = body_start + line_offset + trimmed_end;
        let line_span = span(base, line_start, line_end);
        let tokens = tokenize_line(trimmed, base + line_start as u32);
        let directive = tokens[0].0;

        match directive {
            "module" => {
                if let Some(previous) = &module {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(tokens[0].1, "duplicate 'module' directive")
                        .with_label(
                            Label::secondary(previous.span).with_message("first declared here"),
                        ));
                }
                if tokens.len() < 2 {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(line_span, "'module' requires a path argument"));
                }
                if tokens.len() > 2 {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(tokens[2].1, "unexpected tokens after module path"));
                }
                module = Some(InlineDirectiveValue {
                    value: tokens[1].0.to_string(),
                    span: tokens[1].1,
                });
            }
            "vo" => {
                if let Some(previous) = &vo {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(tokens[0].1, "duplicate 'vo' directive")
                        .with_label(
                            Label::secondary(previous.span).with_message("first declared here"),
                        ));
                }
                if tokens.len() < 2 {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(line_span, "'vo' requires a constraint argument"));
                }
                if tokens.len() > 2 {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(tokens[2].1, "unexpected tokens after vo constraint"));
                }
                vo = Some(InlineDirectiveValue {
                    value: tokens[1].0.to_string(),
                    span: tokens[1].1,
                });
            }
            "require" => {
                if tokens.len() < 3 {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(line_span, "'require' needs <module-path> <constraint>"));
                }
                if tokens.len() > 3 {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(tokens[3].1, "unexpected tokens after require"));
                }
                if let Some(previous) = require
                    .iter()
                    .find(|entry: &&InlineModRequire| entry.module.value == tokens[1].0)
                {
                    return Err(SyntaxError::InvalidInlineMod
                        .at_with_message(
                            tokens[1].1,
                            format!("duplicate require for {}", tokens[1].0),
                        )
                        .with_label(
                            Label::secondary(previous.module.span)
                                .with_message("first required here"),
                        ));
                }
                require.push(InlineModRequire {
                    span: line_span,
                    module: InlineDirectiveValue {
                        value: tokens[1].0.to_string(),
                        span: tokens[1].1,
                    },
                    constraint: InlineDirectiveValue {
                        value: tokens[2].0.to_string(),
                        span: tokens[2].1,
                    },
                });
            }
            "replace" => {
                return Err(SyntaxError::InvalidInlineMod
                    .at_with_message(tokens[0].1, "'replace' is not allowed in inline mod"));
            }
            _ => {
                return Err(SyntaxError::InvalidInlineMod
                    .at_with_message(tokens[0].1, format!("unknown directive '{}'", directive)));
            }
        }

        line_offset += raw_line.len();
    }

    let required_span = if body_span.is_empty() {
        block_span
    } else {
        body_span
    };
    let module = module.ok_or_else(|| {
        SyntaxError::InvalidInlineMod
            .at_with_message(required_span, "inline mod missing 'module' directive")
    })?;
    let vo = vo.ok_or_else(|| {
        SyntaxError::InvalidInlineMod
            .at_with_message(required_span, "inline mod missing 'vo' directive")
    })?;

    Ok(InlineModMetadata {
        span: block_span,
        module,
        vo,
        require,
    })
}

fn tokenize_line(line: &str, line_start: u32) -> Vec<(&str, Span)> {
    let mut tokens = Vec::new();
    let mut token_start = None;

    for (idx, ch) in line.char_indices() {
        if ch.is_whitespace() {
            if let Some(start) = token_start.take() {
                tokens.push((
                    &line[start..idx],
                    Span::from_u32(line_start + start as u32, line_start + idx as u32),
                ));
            }
        } else if token_start.is_none() {
            token_start = Some(idx);
        }
    }

    if let Some(start) = token_start {
        tokens.push((
            &line[start..],
            Span::from_u32(line_start + start as u32, line_start + line.len() as u32),
        ));
    }

    tokens
}

fn trim_bounds(line: &str) -> Option<(usize, usize)> {
    let start = line.find(|c: char| !c.is_whitespace())?;
    let end = line.rfind(|c: char| !c.is_whitespace())? + 1;
    Some((start, end))
}

fn directive_head_len(trimmed: &str) -> usize {
    for (idx, ch) in trimmed.char_indices() {
        if ch.is_whitespace() {
            return idx;
        }
        if trimmed[idx..].starts_with(INLINE_MOD_CLOSE) {
            return idx + INLINE_MOD_CLOSE.len();
        }
    }
    trimmed.len()
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
    fn parses_inline_mod_metadata() {
        let source = "/*vo:mod\nmodule local/demo\nvo ^0.1.0\nrequire github.com/acme/lib ^1.2.0\n*/\npackage main\n";
        let (output, diagnostics) = parse_leading_inline_mod(source, 0);
        assert!(diagnostics.is_empty());
        let inline_mod = output.inline_mod.expect("expected inline mod metadata");
        assert_eq!(inline_mod.module.value, "local/demo");
        assert_eq!(inline_mod.vo.value, "^0.1.0");
        assert_eq!(inline_mod.require.len(), 1);
        assert_eq!(&source[inline_mod.module.span.to_range()], "local/demo");
        assert_eq!(
            &source[inline_mod.require[0].constraint.span.to_range()],
            "^1.2.0"
        );
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
    fn rejects_duplicate_module_with_secondary_label() {
        let source =
            "/*vo:mod\nmodule local/demo\nmodule local/other\nvo ^0.1.0\n*/\npackage main\n";
        let (_, diagnostics) = parse_leading_inline_mod(source, 0);
        assert!(diagnostics.has_errors());
        let diagnostic = diagnostics.iter().next().unwrap();
        assert!(diagnostic.message.contains("duplicate 'module' directive"));
        assert_eq!(diagnostic.labels.len(), 2);
        assert_eq!(&source[diagnostic.labels[1].span.to_range()], "local/demo");
    }

    #[test]
    fn rejects_second_inline_mod_block() {
        let source = "/*vo:mod\nmodule local/a\nvo ^0.1.0\n*/\n/*vo:mod\nmodule local/b\nvo ^0.1.0\n*/\npackage main\n";
        let (_, diagnostics) = parse_leading_inline_mod(source, 0);
        assert!(diagnostics.has_errors());
        let diagnostic = diagnostics.iter().next().unwrap();
        assert!(diagnostic.message.contains("only a single"));
        assert_eq!(diagnostic.labels.len(), 2);
    }
}
