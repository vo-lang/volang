use super::*;

fn lex(source: &str) -> Vec<TokenKind> {
    let lexer = Lexer::new(source, 0);
    let (tokens, _) = lexer.collect_tokens();
    tokens.into_iter().map(|t| t.kind).collect()
}

fn lex_with_errors(source: &str) -> (Vec<TokenKind>, DiagnosticSink) {
    let lexer = Lexer::new(source, 0);
    let (tokens, diags) = lexer.collect_tokens();
    (tokens.into_iter().map(|t| t.kind).collect(), diags)
}

#[test]
fn test_empty() {
    assert_eq!(lex(""), vec![TokenKind::Eof]);
}

#[test]
fn public_lexer_rejects_unrepresentable_source_ranges_without_panicking() {
    let lexer = Lexer::new("x", u32::MAX);
    let (tokens, diagnostics) = lexer.collect_tokens();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].kind, TokenKind::Eof);
    assert!(diagnostics
        .iter()
        .any(|diagnostic| diagnostic.code == Some(SyntaxError::SourceTooLarge.code())));

    #[cfg(target_pointer_width = "64")]
    assert!(matches!(
        source_position_error_for_len(u32::MAX as usize + 1, 0),
        Some(SourcePositionError::LengthTooLarge { .. })
    ));
}

#[test]
fn public_lexer_enforces_the_language_source_file_limit_before_allocation() {
    assert!(source_position_error_for_len(MAX_TEXT_FILE_BYTES, 0).is_none());
    assert!(matches!(
        source_position_error_for_len(MAX_TEXT_FILE_BYTES + 1, 0),
        Some(SourcePositionError::LengthTooLarge {
            limit: MAX_TEXT_FILE_BYTES,
            ..
        })
    ));
}

#[test]
fn public_lexer_accepts_the_last_representable_eof_position() {
    let lexer = Lexer::new("x", u32::MAX - 1);
    let (tokens, diagnostics) = lexer.collect_tokens();
    assert!(diagnostics.is_empty());
    assert_eq!(tokens[0].kind, TokenKind::Ident);
    assert_eq!(tokens[0].span.start.0, u32::MAX - 1);
    assert_eq!(tokens[0].span.end.0, u32::MAX);
    let eof = tokens.last().expect("EOF token");
    assert_eq!(eof.kind, TokenKind::Eof);
    assert_eq!(eof.span.start.0, u32::MAX);
    assert_eq!(eof.span.end.0, u32::MAX);
}

#[test]
fn test_whitespace() {
    assert_eq!(lex("   \t\r  "), vec![TokenKind::Eof]);
}

#[test]
fn test_identifiers() {
    assert_eq!(lex("foo"), vec![TokenKind::Ident, TokenKind::Eof]);
    assert_eq!(lex("_bar"), vec![TokenKind::Ident, TokenKind::Eof]);
    assert_eq!(lex("baz123"), vec![TokenKind::Ident, TokenKind::Eof]);
    assert_eq!(lex("_"), vec![TokenKind::Ident, TokenKind::Eof]);
}

#[test]
fn test_unicode_identifiers_follow_the_language_spec() {
    assert_eq!(
        lex("变量 Δx café２"),
        vec![
            TokenKind::Ident,
            TokenKind::Ident,
            TokenKind::Ident,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn unicode_identifier_boundaries_follow_unicode_16() {
    let (tokens, diagnostics) = lex_with_errors("\u{1e6c0}");
    assert_eq!(tokens, vec![TokenKind::Invalid, TokenKind::Eof]);
    assert!(diagnostics.has_errors());

    let (tokens, diagnostics) = lex_with_errors("x²");
    assert_eq!(
        tokens,
        vec![TokenKind::Ident, TokenKind::Invalid, TokenKind::Eof]
    );
    assert!(diagnostics.has_errors());

    assert_eq!(lex("x２"), vec![TokenKind::Ident, TokenKind::Eof]);
}

#[test]
fn test_keywords() {
    assert_eq!(lex("func"), vec![TokenKind::Func, TokenKind::Eof]);
    assert_eq!(lex("struct"), vec![TokenKind::Struct, TokenKind::Eof]);
    assert_eq!(lex("object"), vec![TokenKind::Ident, TokenKind::Eof]); // object is no longer a keyword
    assert_eq!(lex("return"), vec![TokenKind::Return, TokenKind::Eof]);
}

#[test]
fn test_integers() {
    assert_eq!(lex("0"), vec![TokenKind::IntLit, TokenKind::Eof]);
    assert_eq!(lex("42"), vec![TokenKind::IntLit, TokenKind::Eof]);
    assert_eq!(lex("1_000_000"), vec![TokenKind::IntLit, TokenKind::Eof]);
}

#[test]
fn test_hex_integers() {
    assert_eq!(lex("0x1F"), vec![TokenKind::IntLit, TokenKind::Eof]);
    assert_eq!(lex("0XFF"), vec![TokenKind::IntLit, TokenKind::Eof]);
    assert_eq!(lex("0x1_2_3"), vec![TokenKind::IntLit, TokenKind::Eof]);
}

#[test]
fn test_numeric_separator_placement() {
    for source in ["1_000", "0x1_f", "0o1_7", "0b1_01", "0_644", "1.2_5e1_0"] {
        let (_, diags) = lex_with_errors(source);
        assert!(!diags.has_errors(), "unexpected diagnostics for {source}");
    }

    for source in [
        "1__2", "1_", "0x_1", "0o_7", "0b_1", "0x1_", "0b1__0", ".5_",
    ] {
        let (tokens, diags) = lex_with_errors(source);
        assert!(
            tokens.contains(&TokenKind::Invalid),
            "expected invalid token for {source}"
        );
        assert!(
            diags
                .iter()
                .any(|diag| diag.code == Some(SyntaxError::InvalidNumericSeparator.code())),
            "expected numeric separator diagnostic for {source}"
        );
    }
}

#[test]
fn test_octal_integers() {
    assert_eq!(lex("0o17"), vec![TokenKind::IntLit, TokenKind::Eof]);
    assert_eq!(lex("0O77"), vec![TokenKind::IntLit, TokenKind::Eof]);
}

#[test]
fn test_binary_integers() {
    assert_eq!(lex("0b101"), vec![TokenKind::IntLit, TokenKind::Eof]);
    assert_eq!(lex("0B1111"), vec![TokenKind::IntLit, TokenKind::Eof]);
}

#[test]
fn test_floats() {
    assert_eq!(lex("3.14"), vec![TokenKind::FloatLit, TokenKind::Eof]);
    assert_eq!(lex("1."), vec![TokenKind::FloatLit, TokenKind::Eof]);
    assert_eq!(lex(".5"), vec![TokenKind::FloatLit, TokenKind::Eof]);
    assert_eq!(lex("1e10"), vec![TokenKind::FloatLit, TokenKind::Eof]);
    assert_eq!(lex("1E-5"), vec![TokenKind::FloatLit, TokenKind::Eof]);
    assert_eq!(lex("1.5e+3"), vec![TokenKind::FloatLit, TokenKind::Eof]);
}

#[test]
fn test_hex_floats() {
    assert_eq!(lex("0x1p0"), vec![TokenKind::FloatLit, TokenKind::Eof]);
    assert_eq!(lex("0x1.2p3"), vec![TokenKind::FloatLit, TokenKind::Eof]);
    assert_eq!(lex("0x.8p0"), vec![TokenKind::FloatLit, TokenKind::Eof]);
}

#[test]
fn test_strings() {
    assert_eq!(
        lex(r#""hello""#),
        vec![TokenKind::StringLit, TokenKind::Eof]
    );
    assert_eq!(
        lex(r#""hello\nworld""#),
        vec![TokenKind::StringLit, TokenKind::Eof]
    );
    assert_eq!(lex(r#""""#), vec![TokenKind::StringLit, TokenKind::Eof]);
}

#[test]
fn test_raw_strings() {
    assert_eq!(
        lex("`hello`"),
        vec![TokenKind::RawStringLit, TokenKind::Eof]
    );
    assert_eq!(
        lex("`hello\nworld`"),
        vec![TokenKind::RawStringLit, TokenKind::Eof]
    );
}

#[test]
fn test_runes() {
    assert_eq!(lex("'a'"), vec![TokenKind::RuneLit, TokenKind::Eof]);
    assert_eq!(lex("'\\n'"), vec![TokenKind::RuneLit, TokenKind::Eof]);
    assert_eq!(lex("'\\x41'"), vec![TokenKind::RuneLit, TokenKind::Eof]);
    assert_eq!(lex("'\\u0041'"), vec![TokenKind::RuneLit, TokenKind::Eof]);
}

#[test]
fn test_escape_values_are_in_range() {
    for source in [r#""\uD800""#, r#""\U00110000""#] {
        let (_, diags) = lex_with_errors(source);
        assert!(diags
            .iter()
            .any(|diag| { diag.code == Some(SyntaxError::EscapeInvalidUnicodeScalar.code()) }));
    }

    let (_, diags) = lex_with_errors(r#""\400""#);
    assert!(diags
        .iter()
        .any(|diag| diag.code == Some(SyntaxError::EscapeOctalValue.code())));

    for source in [r#""\U0001F600""#, r#""\377""#] {
        let (_, diags) = lex_with_errors(source);
        assert!(!diags.has_errors(), "unexpected diagnostics for {source}");
    }
}

#[test]
fn test_operators() {
    assert_eq!(lex("+"), vec![TokenKind::Plus, TokenKind::Eof]);
    assert_eq!(lex("-"), vec![TokenKind::Minus, TokenKind::Eof]);
    assert_eq!(lex("*"), vec![TokenKind::Star, TokenKind::Eof]);
    assert_eq!(lex("/"), vec![TokenKind::Slash, TokenKind::Eof]);
    assert_eq!(lex("=="), vec![TokenKind::EqEq, TokenKind::Eof]);
    assert_eq!(lex("!="), vec![TokenKind::NotEq, TokenKind::Eof]);
    assert_eq!(lex("<="), vec![TokenKind::LtEq, TokenKind::Eof]);
    assert_eq!(lex(">="), vec![TokenKind::GtEq, TokenKind::Eof]);
    assert_eq!(lex("&&"), vec![TokenKind::AmpAmp, TokenKind::Eof]);
    assert_eq!(lex("||"), vec![TokenKind::PipePipe, TokenKind::Eof]);
    assert_eq!(lex("<-"), vec![TokenKind::Arrow, TokenKind::Eof]);
    assert_eq!(lex("++"), vec![TokenKind::PlusPlus, TokenKind::Eof]);
    assert_eq!(lex("--"), vec![TokenKind::MinusMinus, TokenKind::Eof]);
    assert_eq!(lex(":="), vec![TokenKind::ColonEq, TokenKind::Eof]);
    assert_eq!(lex("..."), vec![TokenKind::Ellipsis, TokenKind::Eof]);
    assert_eq!(lex("&^"), vec![TokenKind::AmpCaret, TokenKind::Eof]);
}

#[test]
fn test_assignment_operators() {
    assert_eq!(lex("="), vec![TokenKind::Eq, TokenKind::Eof]);
    assert_eq!(lex("+="), vec![TokenKind::PlusEq, TokenKind::Eof]);
    assert_eq!(lex("-="), vec![TokenKind::MinusEq, TokenKind::Eof]);
    assert_eq!(lex("*="), vec![TokenKind::StarEq, TokenKind::Eof]);
    assert_eq!(lex("/="), vec![TokenKind::SlashEq, TokenKind::Eof]);
    assert_eq!(lex("%="), vec![TokenKind::PercentEq, TokenKind::Eof]);
    assert_eq!(lex("<<="), vec![TokenKind::ShlEq, TokenKind::Eof]);
    assert_eq!(lex(">>="), vec![TokenKind::ShrEq, TokenKind::Eof]);
    assert_eq!(lex("&="), vec![TokenKind::AmpEq, TokenKind::Eof]);
    assert_eq!(lex("|="), vec![TokenKind::PipeEq, TokenKind::Eof]);
    assert_eq!(lex("^="), vec![TokenKind::CaretEq, TokenKind::Eof]);
    assert_eq!(lex("&^="), vec![TokenKind::AmpCaretEq, TokenKind::Eof]);
}

#[test]
fn test_delimiters() {
    assert_eq!(
        lex("()"),
        vec![TokenKind::LParen, TokenKind::RParen, TokenKind::Eof]
    );
    assert_eq!(
        lex("[]"),
        vec![TokenKind::LBracket, TokenKind::RBracket, TokenKind::Eof]
    );
    assert_eq!(
        lex("{}"),
        vec![TokenKind::LBrace, TokenKind::RBrace, TokenKind::Eof]
    );
    assert_eq!(lex(","), vec![TokenKind::Comma, TokenKind::Eof]);
    assert_eq!(lex(":"), vec![TokenKind::Colon, TokenKind::Eof]);
    assert_eq!(lex("."), vec![TokenKind::Dot, TokenKind::Eof]);
    assert_eq!(lex(";"), vec![TokenKind::Semicolon, TokenKind::Eof]);
}

#[test]
fn test_semicolon_insertion() {
    // After identifier
    assert_eq!(
        lex("foo\nbar"),
        vec![
            TokenKind::Ident,
            TokenKind::Semicolon,
            TokenKind::Ident,
            TokenKind::Eof
        ]
    );

    // After literal
    assert_eq!(
        lex("42\n"),
        vec![TokenKind::IntLit, TokenKind::Semicolon, TokenKind::Eof]
    );

    // After return
    assert_eq!(
        lex("return\n"),
        vec![TokenKind::Return, TokenKind::Semicolon, TokenKind::Eof]
    );

    // `island` is a complete type expression and may terminate a declaration.
    assert_eq!(
        lex("island\n"),
        vec![TokenKind::Island, TokenKind::Semicolon, TokenKind::Eof]
    );

    // After )
    assert_eq!(
        lex(")\n"),
        vec![TokenKind::RParen, TokenKind::Semicolon, TokenKind::Eof]
    );

    // After }
    assert_eq!(
        lex("}\n"),
        vec![TokenKind::RBrace, TokenKind::Semicolon, TokenKind::Eof]
    );

    // No insertion after +
    assert_eq!(lex("+\n"), vec![TokenKind::Plus, TokenKind::Eof]);

    // No insertion after {
    assert_eq!(lex("{\n"), vec![TokenKind::LBrace, TokenKind::Eof]);
}

#[test]
fn test_line_comments() {
    assert_eq!(lex("// comment"), vec![TokenKind::Eof]);
    assert_eq!(
        lex("foo // comment"),
        vec![TokenKind::Ident, TokenKind::Eof]
    );
    assert_eq!(
        lex("foo // comment\nbar"),
        vec![
            TokenKind::Ident,
            TokenKind::Semicolon,
            TokenKind::Ident,
            TokenKind::Eof
        ]
    );
}

#[test]
fn test_block_comments() {
    assert_eq!(lex("/* comment */"), vec![TokenKind::Eof]);
    assert_eq!(
        lex("foo /* comment */ bar"),
        vec![TokenKind::Ident, TokenKind::Ident, TokenKind::Eof]
    );
    assert_eq!(lex("/* nested /* comment */ */"), vec![TokenKind::Eof]);

    assert_eq!(
        lex("foo /* comment\ncontinues */ bar"),
        vec![
            TokenKind::Ident,
            TokenKind::Semicolon,
            TokenKind::Ident,
            TokenKind::Eof
        ]
    );
    assert_eq!(
        lex("foo + /* comment\ncontinues */ bar"),
        vec![
            TokenKind::Ident,
            TokenKind::Plus,
            TokenKind::Ident,
            TokenKind::Eof
        ]
    );
    assert_eq!(
        lex("foo /* outer\n/* nested */ */ bar"),
        vec![
            TokenKind::Ident,
            TokenKind::Semicolon,
            TokenKind::Ident,
            TokenKind::Eof
        ]
    );
}

#[test]
fn test_inline_mod_block_is_skipped_as_block_comment() {
    // Inline vo.mod metadata per module spec §5.6 is a reserved block
    // comment (`/*vo:mod ... */`). The lexer MUST treat it as an ordinary
    // block comment and produce no tokens for its contents; validation of
    // the inline mod body is owned by the module layer.
    let src = r#"
/*vo:mod
module = "local/demo"
vo = "0.1.0"

[dependencies]
"github.com/vo-lang/vogui" = "^0.4.0"
*/
package main"#;
    assert_eq!(
        lex(src),
        vec![TokenKind::Package, TokenKind::Ident, TokenKind::Eof]
    );
}

#[test]
fn test_complex_expression() {
    let tokens = lex("x := 1 + 2 * 3");
    assert_eq!(
        tokens,
        vec![
            TokenKind::Ident,
            TokenKind::ColonEq,
            TokenKind::IntLit,
            TokenKind::Plus,
            TokenKind::IntLit,
            TokenKind::Star,
            TokenKind::IntLit,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn test_function_declaration() {
    let tokens = lex("func main() int { return 0 }");
    assert_eq!(
        tokens,
        vec![
            TokenKind::Func,
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Ident,
            TokenKind::LBrace,
            TokenKind::Return,
            TokenKind::IntLit,
            TokenKind::RBrace,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn test_error_unterminated_string() {
    let (tokens, diags) = lex_with_errors("\"hello");
    assert!(tokens.contains(&TokenKind::Invalid));
    assert!(diags.has_errors());
}

#[test]
fn test_error_unterminated_rune() {
    let (tokens, diags) = lex_with_errors("'a");
    assert!(tokens.contains(&TokenKind::Invalid));
    assert!(diags.has_errors());
}

#[test]
fn test_error_empty_rune() {
    let (tokens, diags) = lex_with_errors("''");
    assert!(tokens.contains(&TokenKind::Invalid));
    assert!(diags.has_errors());
}

#[test]
fn test_error_invalid_hex() {
    let (tokens, diags) = lex_with_errors("0x");
    assert!(tokens.contains(&TokenKind::Invalid));
    assert!(diags.has_errors());
}

#[test]
fn test_error_invalid_binary() {
    let (tokens, diags) = lex_with_errors("0b");
    assert!(tokens.contains(&TokenKind::Invalid));
    assert!(diags.has_errors());
}

#[test]
fn test_error_invalid_octal() {
    let (tokens, diags) = lex_with_errors("0o");
    assert!(tokens.contains(&TokenKind::Invalid));
    assert!(diags.has_errors());
}

#[test]
fn test_shift_operators() {
    assert_eq!(lex("<<"), vec![TokenKind::Shl, TokenKind::Eof]);
    assert_eq!(lex(">>"), vec![TokenKind::Shr, TokenKind::Eof]);
}

#[test]
fn test_channel_type() {
    // chan<- should be parsed as chan, <-
    let tokens = lex("chan<-");
    assert_eq!(
        tokens,
        vec![TokenKind::Chan, TokenKind::Arrow, TokenKind::Eof]
    );

    // <-chan should be parsed as <-, chan
    let tokens = lex("<-chan");
    assert_eq!(
        tokens,
        vec![TokenKind::Arrow, TokenKind::Chan, TokenKind::Eof]
    );
}

#[test]
fn test_global_positions() {
    // Test that positions are offset by base
    let lexer = Lexer::new("foo", 100);
    let (tokens, _) = lexer.collect_tokens();

    // First token "foo" should be at positions 100-103
    assert_eq!(tokens[0].span.start.0, 100);
    assert_eq!(tokens[0].span.end.0, 103);
}

// ========== Error Handling Tokens ==========

#[test]
fn test_fail_keyword() {
    assert_eq!(lex("fail"), vec![TokenKind::Fail, TokenKind::Eof]);
    // fail is a keyword, not an identifier
    assert_ne!(lex("fail"), vec![TokenKind::Ident, TokenKind::Eof]);
}

#[test]
fn test_errdefer_keyword() {
    assert_eq!(lex("errdefer"), vec![TokenKind::Errdefer, TokenKind::Eof]);
    // errdefer is a keyword, not an identifier
    assert_ne!(lex("errdefer"), vec![TokenKind::Ident, TokenKind::Eof]);
}

#[test]
fn test_island_keyword() {
    assert_eq!(lex("island"), vec![TokenKind::Island, TokenKind::Eof]);
}

#[test]
fn test_question_operator() {
    assert_eq!(lex("?"), vec![TokenKind::Question, TokenKind::Eof]);
}

#[test]
fn test_fail_statement() {
    // fail expr
    assert_eq!(
        lex("fail err"),
        vec![TokenKind::Fail, TokenKind::Ident, TokenKind::Eof]
    );
}

#[test]
fn test_errdefer_statement() {
    // errdefer cleanup()
    assert_eq!(
        lex("errdefer cleanup()"),
        vec![
            TokenKind::Errdefer,
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Eof
        ]
    );
}

#[test]
fn test_try_unwrap_expression() {
    // getValue()?
    assert_eq!(
        lex("getValue()?"),
        vec![
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Question,
            TokenKind::Eof
        ]
    );
}

#[test]
fn test_question_after_call() {
    // result := foo()?
    assert_eq!(
        lex("result := foo()?"),
        vec![
            TokenKind::Ident,
            TokenKind::ColonEq,
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Question,
            TokenKind::Eof
        ]
    );
}

#[test]
fn test_chained_try_unwrap() {
    // a()?.b()?
    assert_eq!(
        lex("a()?.b()?"),
        vec![
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Question,
            TokenKind::Dot,
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Question,
            TokenKind::Eof
        ]
    );
}

#[test]
fn test_fail_with_error_literal() {
    // fail errors.New("failed")
    assert_eq!(
        lex(r#"fail errors.New("failed")"#),
        vec![
            TokenKind::Fail,
            TokenKind::Ident,
            TokenKind::Dot,
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::StringLit,
            TokenKind::RParen,
            TokenKind::Eof
        ]
    );
}

#[test]
fn test_error_handling_in_function() {
    // Complete error handling pattern
    let code = r#"func foo() error {
    errdefer cleanup()
    result := bar()?
    if result == nil {
        fail ErrNotFound
    }
    return nil
}"#;
    let tokens = lex(code);

    // Check key tokens are present
    assert!(tokens.contains(&TokenKind::Errdefer));
    assert!(tokens.contains(&TokenKind::Question));
    assert!(tokens.contains(&TokenKind::Fail));
}

#[test]
fn test_fail_not_identifier_prefix() {
    // "failure" should be an identifier, not fail + ure
    assert_eq!(lex("failure"), vec![TokenKind::Ident, TokenKind::Eof]);
    assert_eq!(lex("failing"), vec![TokenKind::Ident, TokenKind::Eof]);
}

#[test]
fn test_errdefer_not_identifier_prefix() {
    // "errdeferred" should be an identifier
    assert_eq!(lex("errdeferred"), vec![TokenKind::Ident, TokenKind::Eof]);
}
