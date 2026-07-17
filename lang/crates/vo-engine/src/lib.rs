//! Vo Compilation and Execution Core
//!
//! This crate provides the core compile and run functionality for Vo programs.
//! It is used by both the Vo CLI launcher and the vox library.

mod compile;
mod format;
mod run;
mod scan;
mod toolchain;

pub use compile::{
    check, check_path, check_path_with_auto_install, check_path_with_auto_install_with_options,
    check_path_with_options, check_with_auto_install, check_with_auto_install_with_options,
    check_with_options, compile, compile_from_memory, compile_path, compile_path_with_auto_install,
    compile_source_at, compile_string, compile_with_auto_install,
    compile_with_auto_install_with_options, compile_with_cache, compile_with_cache_with_options,
    compile_with_options, default_mod_cache_root, prepare_native_extension_specs,
    with_compile_log_sink, CompileError, CompileLogRecord, CompileOutput, ModuleSystemError,
    ModuleSystemErrorKind, ModuleSystemStage, PreparedNativeExtension,
};
pub use format::format_text;
pub use run::{
    build_gui_vm, run, run_with_byte_args, run_with_output, run_with_output_interruptible,
    run_with_output_interruptible_observed, run_with_output_observed, RunError, RunMode,
    RunObservation, RuntimeError, RuntimeErrorKind,
};
pub use scan::scan_external_imports;
pub use toolchain::ensure_toolchain_host_installed;

/// Format Vo source code, returning the formatted string.
/// Returns an error if the source has parse errors.
pub fn format_source(source: &str) -> Result<String, String> {
    let formatted = format_source_once(source)?;
    let reformatted = format_source_once(&formatted)?;
    if reformatted != formatted {
        return Err("formatter produced non-idempotent output; source was left unchanged".into());
    }
    Ok(formatted)
}

fn format_source_once(source: &str) -> Result<String, String> {
    let (file, diags, interner) = vo_syntax::parser::parse(source, 0);
    if diags.has_errors() {
        return Err(format_diagnostics(&diags));
    }

    let inline_mod_span = file
        .inline_mod
        .as_ref()
        .map(|metadata| metadata.span.to_range());
    let canonical = vo_syntax::display::format_file(&file, &interner);
    let mut formatted = canonical.clone();
    if let Some(span) = &inline_mod_span {
        preserve_inline_mod_text(source, span.clone(), &mut formatted)?;
    }
    formatted = restore_comments(source, &formatted, inline_mod_span.as_ref())?;
    formatted = normalize_formatted_layout(&formatted);

    let (formatted_file, formatted_diags, formatted_interner) =
        vo_syntax::parser::parse(&formatted, 0);
    if formatted_diags.has_errors() {
        return Err(format!(
            "formatter produced invalid source: {}",
            format_diagnostics(&formatted_diags)
        ));
    }
    let reparsed_canonical = vo_syntax::display::format_file(&formatted_file, &formatted_interner);
    if reparsed_canonical != canonical {
        return Err(
            "formatter comment placement changed the parsed program; source was left unchanged"
                .into(),
        );
    }
    Ok(formatted)
}

fn format_diagnostics(diags: &vo_common::diagnostics::DiagnosticSink) -> String {
    diags
        .iter()
        .map(|diag| diag.message.as_str())
        .collect::<Vec<_>>()
        .join("; ")
}

fn preserve_inline_mod_text(
    source: &str,
    source_span: std::ops::Range<usize>,
    formatted: &mut String,
) -> Result<(), String> {
    let Some(comment) = scan_comments(formatted)
        .into_iter()
        .find(|comment| formatted[comment.start..comment.end].starts_with("/*vo:mod"))
    else {
        return Err("formatter failed to serialize inline module metadata".into());
    };
    formatted.replace_range(comment.start..comment.end, &source[source_span]);
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CommentKind {
    Line,
    Block,
}

#[derive(Debug, Clone, Copy)]
struct SourceComment {
    start: usize,
    end: usize,
    kind: CommentKind,
}

fn scan_comments(source: &str) -> Vec<SourceComment> {
    let bytes = source.as_bytes();
    let mut comments = Vec::new();
    let mut index = 0;

    while index < bytes.len() {
        match bytes[index] {
            b'"' | b'\'' => {
                let quote = bytes[index];
                index += 1;
                while index < bytes.len() {
                    if bytes[index] == b'\\' {
                        index = (index + 2).min(bytes.len());
                    } else if bytes[index] == quote {
                        index += 1;
                        break;
                    } else {
                        index += 1;
                    }
                }
            }
            b'`' => {
                index += 1;
                while index < bytes.len() && bytes[index] != b'`' {
                    index += 1;
                }
                index = (index + 1).min(bytes.len());
            }
            b'/' if bytes.get(index + 1) == Some(&b'/') => {
                let start = index;
                index += 2;
                while index < bytes.len() && bytes[index] != b'\n' {
                    index += 1;
                }
                comments.push(SourceComment {
                    start,
                    end: index,
                    kind: CommentKind::Line,
                });
            }
            b'/' if bytes.get(index + 1) == Some(&b'*') => {
                let start = index;
                index += 2;
                let mut depth = 1usize;
                while index < bytes.len() && depth > 0 {
                    if bytes.get(index..index + 2) == Some(b"/*") {
                        depth += 1;
                        index += 2;
                    } else if bytes.get(index..index + 2) == Some(b"*/") {
                        depth -= 1;
                        index += 2;
                    } else {
                        index += 1;
                    }
                }
                comments.push(SourceComment {
                    start,
                    end: index,
                    kind: CommentKind::Block,
                });
            }
            _ => index += 1,
        }
    }
    comments
}

#[derive(Debug)]
struct LexToken<'a> {
    kind: vo_syntax::TokenKind,
    text: &'a str,
    start: usize,
    end: usize,
}

fn lex_tokens(source: &str) -> Vec<LexToken<'_>> {
    let (tokens, _) = vo_syntax::Lexer::new(source, 0).collect_tokens();
    tokens
        .into_iter()
        .filter(|token| {
            token.kind != vo_syntax::TokenKind::Eof && token.span.start != token.span.end
        })
        .map(|token| {
            let range = token.span.to_range();
            LexToken {
                kind: token.kind,
                text: &source[range.clone()],
                start: range.start,
                end: range.end,
            }
        })
        .collect()
}

fn token_matches(left: &LexToken<'_>, right: &LexToken<'_>) -> bool {
    left.kind == right.kind
        && (!left.kind.is_literal() && left.kind != vo_syntax::TokenKind::Ident
            || left.text == right.text)
}

fn align_tokens(source: &[LexToken<'_>], formatted: &[LexToken<'_>]) -> Vec<Option<usize>> {
    const LOOKAHEAD: usize = 48;
    let mut mapping = vec![None; source.len()];
    let (mut source_index, mut formatted_index) = (0usize, 0usize);

    while source_index < source.len() && formatted_index < formatted.len() {
        if token_matches(&source[source_index], &formatted[formatted_index]) {
            mapping[source_index] = Some(formatted_index);
            source_index += 1;
            formatted_index += 1;
            continue;
        }

        let mut best: Option<(usize, usize)> = None;
        let source_limit = (source_index + LOOKAHEAD).min(source.len() - 1);
        let formatted_limit = (formatted_index + LOOKAHEAD).min(formatted.len() - 1);
        for (source_offset, source_token) in source[source_index..=source_limit].iter().enumerate()
        {
            let next_source = source_index + source_offset;
            for (formatted_offset, formatted_token) in formatted[formatted_index..=formatted_limit]
                .iter()
                .enumerate()
            {
                let next_formatted = formatted_index + formatted_offset;
                if !token_matches(source_token, formatted_token) {
                    continue;
                }
                let distance = next_source - source_index + next_formatted - formatted_index;
                if best.is_none_or(|(old_source, old_formatted)| {
                    distance < old_source - source_index + old_formatted - formatted_index
                }) {
                    best = Some((next_source, next_formatted));
                }
            }
        }

        let Some((next_source, next_formatted)) = best else {
            break;
        };
        source_index = next_source;
        formatted_index = next_formatted;
    }
    mapping
}

#[derive(Debug)]
struct CommentInsertion {
    offset: usize,
    remove_until: usize,
    order: usize,
    text: String,
}

fn restore_comments(
    source: &str,
    formatted: &str,
    excluded_span: Option<&std::ops::Range<usize>>,
) -> Result<String, String> {
    let comments: Vec<_> = scan_comments(source)
        .into_iter()
        .filter(|comment| {
            !excluded_span
                .is_some_and(|span| comment.start >= span.start && comment.end <= span.end)
        })
        .collect();
    if comments.is_empty() {
        return Ok(formatted.to_string());
    }

    let source_tokens = lex_tokens(source);
    let formatted_tokens = lex_tokens(formatted);
    let mapping = align_tokens(&source_tokens, &formatted_tokens);
    let mut insertions = Vec::with_capacity(comments.len());

    for (order, comment) in comments.iter().enumerate() {
        let previous = source_tokens
            .iter()
            .enumerate()
            .rev()
            .find(|(index, token)| token.end <= comment.start && mapping[*index].is_some());
        let next = source_tokens
            .iter()
            .enumerate()
            .find(|(index, token)| token.start >= comment.end && mapping[*index].is_some());
        if previous.is_none() && next.is_none() && !source_tokens.is_empty() {
            return Err("formatter could not anchor every source comment safely".into());
        }

        let raw = &source[comment.start..comment.end];
        let line_start = source[..comment.start]
            .rfind('\n')
            .map_or(0, |position| position + 1);
        let line_end = source[comment.end..]
            .find('\n')
            .map_or(source.len(), |position| comment.end + position);
        let has_code_before = !source[line_start..comment.start].trim().is_empty();
        let has_code_after = !source[comment.end..line_end].trim().is_empty();
        let source_comment_indent = source[line_start..comment.start]
            .chars()
            .take_while(|c| matches!(c, ' ' | '\t'))
            .collect::<String>();

        let previous_offset = previous
            .and_then(|(index, _)| mapping[index].map(|mapped| formatted_tokens[mapped].end));
        let next_offset =
            next.and_then(|(index, _)| mapping[index].map(|mapped| formatted_tokens[mapped].start));
        let restore_unmapped_comma = source_tokens
            .iter()
            .enumerate()
            .rev()
            .find(|(_, token)| token.end <= comment.start)
            .is_some_and(|(index, token)| {
                token.kind == vo_syntax::TokenKind::Comma && mapping[index].is_none()
            });

        let (offset, remove_until, text) = match (comment.kind, has_code_before, has_code_after) {
            (CommentKind::Line, true, _) | (CommentKind::Block, true, false) => {
                let offset = previous_offset.or(next_offset).unwrap_or(formatted.len());
                let mut text = if restore_unmapped_comma {
                    format!(", {raw}")
                } else {
                    format!(" {raw}")
                };
                let next_token_is_on_same_line = next_offset
                    .is_some_and(|next| next >= offset && !formatted[offset..next].contains('\n'));
                let mut remove_until = offset;
                if next_token_is_on_same_line || !formatted[offset..].contains('\n') {
                    text.push('\n');
                    let indent = next
                        .map(|(_, token)| indent_at(source, token.start))
                        .unwrap_or_else(|| indent_at(formatted, offset));
                    text.push_str(&indent);
                    if next_token_is_on_same_line {
                        remove_until = next_offset.unwrap_or(offset);
                    }
                }
                (offset, remove_until, text)
            }
            (CommentKind::Block, true, true) => {
                let offset = previous_offset.or(next_offset).unwrap_or(formatted.len());
                (offset, offset, format!(" {raw} "))
            }
            (CommentKind::Block, false, true) => {
                let offset = next_offset.or(previous_offset).unwrap_or(0);
                (offset, offset, format!("{raw} "))
            }
            _ => {
                let offset = next_offset.or(previous_offset).unwrap_or(0);
                let next_indent = next
                    .and_then(|(index, _)| mapping[index])
                    .map(|mapped| indent_at(formatted, formatted_tokens[mapped].start))
                    .unwrap_or_default();
                let previous_indent = previous
                    .and_then(|(index, _)| mapping[index])
                    .map(|mapped| indent_at(formatted, formatted_tokens[mapped].start))
                    .unwrap_or_default();
                let comment_indent = [source_comment_indent, previous_indent, next_indent.clone()]
                    .into_iter()
                    .max_by_key(|indent| indent_width(indent))
                    .unwrap_or_default();
                let line_start = formatted[..offset]
                    .rfind('\n')
                    .map_or(0, |position| position + 1);
                let needs_leading_newline = !formatted[line_start..offset].trim().is_empty();
                let mut text = String::new();
                if needs_leading_newline {
                    text.push('\n');
                    text.push_str(&comment_indent);
                } else if let Some(extra_indent) = comment_indent.strip_prefix(&next_indent) {
                    // The canonical indentation before the next token has
                    // already been copied. Add only the depth needed for a
                    // comment that belongs inside a closing delimiter.
                    text.push_str(extra_indent);
                }
                text.push_str(raw);
                text.push('\n');
                text.push_str(&next_indent);
                (offset, offset, text)
            }
        };
        insertions.push(CommentInsertion {
            offset,
            remove_until,
            order,
            text,
        });
    }

    insertions.sort_by_key(|insertion| (insertion.offset, insertion.order));
    let mut output = String::with_capacity(
        formatted.len() + insertions.iter().map(|item| item.text.len()).sum::<usize>(),
    );
    let mut cursor = 0usize;
    for insertion in insertions {
        if insertion.offset < cursor || insertion.offset > formatted.len() {
            return Err("formatter could not preserve comment placement safely".into());
        }
        output.push_str(&formatted[cursor..insertion.offset]);
        output.push_str(&insertion.text);
        cursor = insertion.remove_until;
    }
    output.push_str(&formatted[cursor..]);
    Ok(output)
}

fn indent_at(source: &str, offset: usize) -> String {
    let line_start = source[..offset]
        .rfind('\n')
        .map_or(0, |position| position + 1);
    source[line_start..offset]
        .chars()
        .take_while(|c| matches!(c, ' ' | '\t'))
        .collect()
}

fn indent_width(indent: &str) -> usize {
    indent
        .chars()
        .map(|character| if character == '\t' { 4 } else { 1 })
        .sum()
}

#[derive(Debug, Clone, Copy)]
enum LayoutMode {
    Code,
    Quoted { quote: u8, escaped: bool },
    RawString,
    LineComment,
    BlockComment { depth: usize },
}

/// Removes line-ending whitespace outside raw string literals and guarantees
/// exactly one final newline. Comment text is preserved apart from whitespace
/// that would otherwise remain at the end of a physical line.
fn normalize_formatted_layout(source: &str) -> String {
    let bytes = source.as_bytes();
    let mut output = Vec::with_capacity(bytes.len() + 1);
    let mut mode = LayoutMode::Code;
    let mut index = 0usize;

    while index < bytes.len() {
        let byte = bytes[index];
        if byte == b'\n' {
            if !matches!(mode, LayoutMode::RawString) {
                while matches!(output.last(), Some(b' ' | b'\t' | b'\r')) {
                    output.pop();
                }
            }
            output.push(b'\n');
            if matches!(mode, LayoutMode::LineComment) {
                mode = LayoutMode::Code;
            }
            index += 1;
            continue;
        }

        match mode {
            LayoutMode::Code => match byte {
                b'"' | b'\'' => {
                    mode = LayoutMode::Quoted {
                        quote: byte,
                        escaped: false,
                    };
                    output.push(byte);
                    index += 1;
                }
                b'`' => {
                    mode = LayoutMode::RawString;
                    output.push(byte);
                    index += 1;
                }
                b'/' if bytes.get(index + 1) == Some(&b'/') => {
                    mode = LayoutMode::LineComment;
                    output.extend_from_slice(b"//");
                    index += 2;
                }
                b'/' if bytes.get(index + 1) == Some(&b'*') => {
                    mode = LayoutMode::BlockComment { depth: 1 };
                    output.extend_from_slice(b"/*");
                    index += 2;
                }
                _ => {
                    output.push(byte);
                    index += 1;
                }
            },
            LayoutMode::Quoted { quote, escaped } => {
                output.push(byte);
                index += 1;
                mode = if escaped {
                    LayoutMode::Quoted {
                        quote,
                        escaped: false,
                    }
                } else if byte == b'\\' {
                    LayoutMode::Quoted {
                        quote,
                        escaped: true,
                    }
                } else if byte == quote {
                    LayoutMode::Code
                } else {
                    LayoutMode::Quoted {
                        quote,
                        escaped: false,
                    }
                };
            }
            LayoutMode::RawString => {
                output.push(byte);
                index += 1;
                if byte == b'`' {
                    mode = LayoutMode::Code;
                }
            }
            LayoutMode::LineComment => {
                output.push(byte);
                index += 1;
            }
            LayoutMode::BlockComment { mut depth } => {
                if byte == b'/' && bytes.get(index + 1) == Some(&b'*') {
                    depth += 1;
                    output.extend_from_slice(b"/*");
                    index += 2;
                    mode = LayoutMode::BlockComment { depth };
                } else if byte == b'*' && bytes.get(index + 1) == Some(&b'/') {
                    depth -= 1;
                    output.extend_from_slice(b"*/");
                    index += 2;
                    mode = if depth == 0 {
                        LayoutMode::Code
                    } else {
                        LayoutMode::BlockComment { depth }
                    };
                } else {
                    output.push(byte);
                    index += 1;
                }
            }
        }
    }

    while matches!(output.last(), Some(b' ' | b'\t' | b'\r' | b'\n')) {
        output.pop();
    }
    if !output.is_empty() {
        output.push(b'\n');
    }
    String::from_utf8(output).expect("formatter layout normalization preserves UTF-8")
}

pub use vo_runtime::output::CaptureSink;
pub use vo_vm::bytecode::Module;

#[cfg(test)]
mod formatter_tests {
    use super::format_source;

    #[test]
    fn preserves_comments_and_produces_idempotent_source() {
        let source = r#"package main

// declaration comment
func main(){
    x:=1 // trailing comment
    /* statement comment */
    _=x /* inline comment */
}
"#;
        let formatted = format_source(source).expect("format source");
        for comment in [
            "// declaration comment",
            "// trailing comment",
            "/* statement comment */",
            "/* inline comment */",
        ] {
            assert!(
                formatted.contains(comment),
                "missing {comment:?}\n{formatted}"
            );
        }
        assert_eq!(format_source(&formatted).unwrap(), formatted);
    }

    #[test]
    fn preserves_inline_mod_text_and_comments() {
        let inline_mod =
            "/*vo:mod\n# keep this module note\nmodule = \"local/demo\"\nvo = \"^0.1.0\"\n*/";
        let source =
            format!("{inline_mod}\npackage main\n\n// keep declaration note\nfunc main(){{}}\n");
        let formatted = format_source(&source).expect("format inline module source");
        assert!(formatted.starts_with(inline_mod));
        assert!(formatted.contains("// keep declaration note"));
    }

    #[test]
    fn preserves_comments_around_grouped_import_rewrite() {
        let source = r#"package main
// first import
import "fmt"
// second import
import "strings"
func main(){fmt.Println(strings.TrimSpace(" x "))}
"#;
        let formatted = format_source(source).expect("format imports");
        assert!(formatted.contains("// first import"));
        assert!(formatted.contains("// second import"));
        let (_, diags, _) = vo_syntax::parser::parse(&formatted, 0);
        assert!(!diags.has_errors(), "{formatted}");
    }

    #[test]
    fn type_switch_output_reparses() {
        let source = "package main\nfunc main(){var x any\nswitch v:=x.(type){case int:_=v}}\n";
        let formatted = format_source(source).expect("format type switch");
        assert_eq!(formatted.matches(".(type)").count(), 1);
    }

    #[test]
    fn preserves_composite_literal_line_comments_and_reparses() {
        let source = r#"package main

func main() {
	values := []int{
		1, // first element
		2, // second element
	}
	_ = values
}
"#;
        let once = super::format_source_once(source).expect("first formatter pass");
        let twice = super::format_source_once(&once).expect("second formatter pass");
        assert_eq!(twice, once, "first:\n{once}\nsecond:\n{twice}");
        let formatted = format_source(source).expect("format composite literal comments");
        assert!(formatted.contains("1, // first element\n"), "{formatted}");
        assert!(formatted.contains("2, // second element\n"), "{formatted}");
        let (_, diagnostics, _) = vo_syntax::parser::parse(&formatted, 0);
        assert!(!diagnostics.has_errors(), "{formatted}");
        assert_eq!(format_source(&formatted).unwrap(), formatted);
    }

    #[test]
    fn removes_trailing_layout_whitespace_and_extra_eof_lines() {
        let source = "package main   \n\nfunc main() { // note   \n}\n\n\n";
        let formatted = format_source(source).expect("format trailing whitespace");
        assert!(formatted.ends_with('\n'));
        assert!(!formatted.ends_with("\n\n"), "{formatted:?}");
        assert!(formatted.lines().all(|line| !line.ends_with([' ', '\t'])));
        assert_eq!(format_source(&formatted).unwrap(), formatted);
    }

    #[test]
    fn preserves_raw_string_line_ending_spaces() {
        let source =
            "package main\n\nfunc main() {\n\tvalue := `first  \nsecond  `\n\t_ = value\n}\n";
        let formatted = format_source(source).expect("format raw string");
        assert!(formatted.contains("`first  \nsecond  `"), "{formatted:?}");
        assert_eq!(format_source(&formatted).unwrap(), formatted);
    }

    #[test]
    fn keeps_standalone_comment_at_enclosing_block_depth() {
        let source = r#"package main

func main() {
	work()
	// The canceled worker must also exit promptly.
}
"#;
        let formatted = format_source(source).expect("format standalone block comment");
        assert!(
            formatted.contains("\n\t// The canceled worker must also exit promptly.\n}"),
            "{formatted}"
        );
        assert_eq!(format_source(&formatted).unwrap(), formatted);
    }
}
