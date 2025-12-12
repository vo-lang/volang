//! Comprehensive tests for the GoX lexer and parser.

use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser;
use crate::token::TokenKind;

// ═══════════════════════════════════════════════════════════════════════════
// Helper functions
// ═══════════════════════════════════════════════════════════════════════════

fn tokenize(input: &str) -> Vec<TokenKind> {
    Lexer::new(input).map(|t| t.kind).collect()
}

fn assert_single(input: &str, expected: TokenKind) {
    let tokens = tokenize(input);
    assert_eq!(
        tokens.len(),
        1,
        "Expected 1 token for {:?}, got {:?}",
        input,
        tokens
    );
    assert_eq!(tokens[0], expected);
}

fn assert_ident(input: &str, expected: &str) {
    let tokens = tokenize(input);
    assert_eq!(tokens.len(), 1);
    assert!(matches!(&tokens[0], TokenKind::Ident(s) if s == expected));
}

fn assert_string(input: &str, expected: &str) {
    let tokens = tokenize(input);
    assert_eq!(tokens.len(), 1);
    assert!(matches!(&tokens[0], TokenKind::String(s) if s == expected));
}

// ═══════════════════════════════════════════════════════════════════════════
// Basic Token Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_empty_input() {
    assert!(tokenize("").is_empty());
}

#[test]
fn test_whitespace_only() {
    assert!(tokenize("   \t\n  ").is_empty());
}

#[test]
fn test_single_identifier() {
    assert_ident("foo", "foo");
}

#[test]
fn test_identifier_with_underscore() {
    assert_ident("_foo", "_foo");
    assert_ident("foo_bar", "foo_bar");
    assert_ident("_", "_");
    assert_ident("__init__", "__init__");
}

#[test]
fn test_identifier_with_numbers() {
    assert_ident("foo123", "foo123");
    assert_ident("x1y2z3", "x1y2z3");
}

// ═══════════════════════════════════════════════════════════════════════════
// Keyword Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_all_keywords() {
    assert_single("package", TokenKind::Package);
    assert_single("import", TokenKind::Import);
    assert_single("var", TokenKind::Var);
    assert_single("const", TokenKind::Const);
    assert_single("type", TokenKind::Type);
    assert_single("func", TokenKind::Func);
    assert_single("interface", TokenKind::Interface);
    assert_single("implements", TokenKind::Implements);
    assert_single("struct", TokenKind::Struct);
    assert_single("map", TokenKind::Map);
    assert_single("if", TokenKind::If);
    assert_single("else", TokenKind::Else);
    assert_single("for", TokenKind::For);
    assert_single("switch", TokenKind::Switch);
    assert_single("case", TokenKind::Case);
    assert_single("default", TokenKind::Default);
    assert_single("return", TokenKind::Return);
    assert_single("break", TokenKind::Break);
    assert_single("continue", TokenKind::Continue);
    assert_single("true", TokenKind::True);
    assert_single("false", TokenKind::False);
    assert_single("nil", TokenKind::Nil);
}

#[test]
fn test_keyword_as_prefix() {
    // Keywords followed by more chars become identifiers
    assert_ident("packages", "packages");
    assert_ident("iff", "iff");
    assert_ident("returning", "returning");
}

// ═══════════════════════════════════════════════════════════════════════════
// Number Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_integers() {
    assert_single("0", TokenKind::Int(0));
    assert_single("42", TokenKind::Int(42));
    assert_single("123456789", TokenKind::Int(123456789));
}

#[test]
fn test_floats() {
    let tokens = tokenize("3.15");
    assert!(matches!(&tokens[0], TokenKind::Float(f) if (*f - 3.15).abs() < 0.001));

    let tokens = tokenize("0.5");
    assert!(matches!(&tokens[0], TokenKind::Float(f) if (*f - 0.5).abs() < 0.001));

    let tokens = tokenize("123.456");
    assert!(matches!(&tokens[0], TokenKind::Float(f) if (*f - 123.456).abs() < 0.001));
}

#[test]
fn test_integer_followed_by_dot() {
    // `42.foo` should be Int(42), Dot, Ident("foo")
    let tokens = tokenize("42.foo");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], TokenKind::Int(42));
    assert_eq!(tokens[1], TokenKind::Dot);
    assert!(matches!(&tokens[2], TokenKind::Ident(s) if s == "foo"));
}

// ═══════════════════════════════════════════════════════════════════════════
// String Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_empty_string() {
    assert_string(r#""""#, "");
}

#[test]
fn test_simple_string() {
    assert_string(r#""hello""#, "hello");
}

#[test]
fn test_string_with_spaces() {
    assert_string(r#""hello world""#, "hello world");
}

#[test]
fn test_string_escape_sequences() {
    assert_string(r#""hello\nworld""#, "hello\nworld");
    assert_string(r#""tab\there""#, "tab\there");
    assert_string(r#""back\\slash""#, "back\\slash");
    assert_string(r#""quote\"here""#, "quote\"here");
}

#[test]
fn test_unterminated_string() {
    assert_single(r#""hello"#, TokenKind::UnterminatedString);
}

#[test]
fn test_string_with_newline() {
    // Newline in string should produce unterminated string error
    // After the unterminated string, lexer continues and finds "world"
    let tokens = tokenize("\"hello\nworld\"");
    assert!(matches!(&tokens[0], TokenKind::UnterminatedString));
}

#[test]
fn test_unicode_in_string() {
    assert_string(r#""Hello, 世界""#, "Hello, 世界");
}

// ═══════════════════════════════════════════════════════════════════════════
// Operator Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_arithmetic_operators() {
    assert_single("+", TokenKind::Plus);
    assert_single("-", TokenKind::Minus);
    assert_single("*", TokenKind::Star);
    assert_single("/", TokenKind::Slash);
    assert_single("%", TokenKind::Percent);
}

#[test]
fn test_comparison_operators() {
    assert_single("==", TokenKind::Eq);
    assert_single("!=", TokenKind::NotEq);
    assert_single("<", TokenKind::Lt);
    assert_single("<=", TokenKind::LtEq);
    assert_single(">", TokenKind::Gt);
    assert_single(">=", TokenKind::GtEq);
}

#[test]
fn test_logical_operators() {
    assert_single("&&", TokenKind::And);
    assert_single("||", TokenKind::Or);
    assert_single("!", TokenKind::Not);
}

#[test]
fn test_assignment_operators() {
    assert_single("=", TokenKind::Assign);
    assert_single(":=", TokenKind::ColonAssign);
    assert_single("+=", TokenKind::PlusAssign);
    assert_single("-=", TokenKind::MinusAssign);
    assert_single("*=", TokenKind::StarAssign);
    assert_single("/=", TokenKind::SlashAssign);
    assert_single("%=", TokenKind::PercentAssign);
}

#[test]
fn test_invalid_single_ampersand() {
    assert_single("&", TokenKind::Invalid('&'));
}

#[test]
fn test_invalid_single_pipe() {
    assert_single("|", TokenKind::Invalid('|'));
}

// ═══════════════════════════════════════════════════════════════════════════
// Delimiter Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_delimiters() {
    assert_single("(", TokenKind::LParen);
    assert_single(")", TokenKind::RParen);
    assert_single("[", TokenKind::LBracket);
    assert_single("]", TokenKind::RBracket);
    assert_single("{", TokenKind::LBrace);
    assert_single("}", TokenKind::RBrace);
    assert_single(",", TokenKind::Comma);
    assert_single(":", TokenKind::Colon);
    assert_single(";", TokenKind::Semi);
    assert_single(".", TokenKind::Dot);
}

// ═══════════════════════════════════════════════════════════════════════════
// Comment Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_line_comment() {
    let tokens = tokenize("x // comment");
    assert_eq!(tokens.len(), 1);
    assert!(matches!(&tokens[0], TokenKind::Ident(s) if s == "x"));
}

#[test]
fn test_line_comment_only() {
    assert!(tokenize("// just a comment").is_empty());
}

#[test]
fn test_block_comment() {
    let tokens = tokenize("x /* comment */ y");
    assert_eq!(tokens.len(), 2);
    assert!(matches!(&tokens[0], TokenKind::Ident(s) if s == "x"));
    assert!(matches!(&tokens[1], TokenKind::Ident(s) if s == "y"));
}

#[test]
fn test_multiline_block_comment() {
    let input = r#"x /* line 1
    line 2
    line 3 */ y"#;
    let tokens = tokenize(input);
    // x triggers semi insertion after the multiline comment ends
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[0], TokenKind::Ident(s) if s == "x"));
    assert_eq!(tokens[1], TokenKind::Semi);
    assert!(matches!(&tokens[2], TokenKind::Ident(s) if s == "y"));
}

#[test]
fn test_nested_looking_comment() {
    // GoX doesn't support nested comments, so inner /* is ignored
    let tokens = tokenize("x /* outer /* inner */ y");
    assert_eq!(tokens.len(), 2);
}

// ═══════════════════════════════════════════════════════════════════════════
// Semicolon Insertion Tests (§3.6)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_semi_after_ident() {
    let tokens = tokenize("x\ny");
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[0], TokenKind::Ident(s) if s == "x"));
    assert_eq!(tokens[1], TokenKind::Semi);
    assert!(matches!(&tokens[2], TokenKind::Ident(s) if s == "y"));
}

#[test]
fn test_semi_after_literal() {
    let tokens = tokenize("42\nx");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], TokenKind::Int(42));
    assert_eq!(tokens[1], TokenKind::Semi);
}

#[test]
fn test_semi_after_return() {
    let tokens = tokenize("return\nx");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], TokenKind::Return);
    assert_eq!(tokens[1], TokenKind::Semi);
}

#[test]
fn test_semi_after_break() {
    let tokens = tokenize("break\nx");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], TokenKind::Break);
    assert_eq!(tokens[1], TokenKind::Semi);
}

#[test]
fn test_semi_after_continue() {
    let tokens = tokenize("continue\nx");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], TokenKind::Continue);
    assert_eq!(tokens[1], TokenKind::Semi);
}

#[test]
fn test_semi_after_true_false_nil() {
    let tokens = tokenize("true\nfalse");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], TokenKind::True);
    assert_eq!(tokens[1], TokenKind::Semi);
    assert_eq!(tokens[2], TokenKind::False);

    let tokens = tokenize("nil\nx");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], TokenKind::Nil);
    assert_eq!(tokens[1], TokenKind::Semi);
}

#[test]
fn test_semi_after_closing_delimiters() {
    let tokens = tokenize(")\nx");
    assert_eq!(tokens[0], TokenKind::RParen);
    assert_eq!(tokens[1], TokenKind::Semi);

    let tokens = tokenize("]\nx");
    assert_eq!(tokens[0], TokenKind::RBracket);
    assert_eq!(tokens[1], TokenKind::Semi);

    let tokens = tokenize("}\nx");
    assert_eq!(tokens[0], TokenKind::RBrace);
    assert_eq!(tokens[1], TokenKind::Semi);
}

#[test]
fn test_no_semi_after_operators() {
    // No semicolon should be inserted after operators
    // But the following `x` on a new line gets a semi after it if followed by more tokens
    let tokens = tokenize("+\nx");
    // + (no semi), x followed by EOF (no semi)
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0], TokenKind::Plus);
    assert!(matches!(&tokens[1], TokenKind::Ident(s) if s == "x"));
}

#[test]
fn test_no_semi_after_keywords_except_special() {
    // No semicolon after `if`, `else`, `for`, etc.
    let tokens = tokenize("if\nx");
    // if (no semi), x followed by EOF (no semi)
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0], TokenKind::If);
    assert!(matches!(&tokens[1], TokenKind::Ident(s) if s == "x"));
}

// ═══════════════════════════════════════════════════════════════════════════
// Complex Expression Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_var_declaration() {
    let tokens = tokenize("var x int = 42;");
    assert_eq!(tokens.len(), 6);
    assert_eq!(tokens[0], TokenKind::Var);
    assert!(matches!(&tokens[1], TokenKind::Ident(s) if s == "x"));
    assert!(matches!(&tokens[2], TokenKind::Ident(s) if s == "int"));
    assert_eq!(tokens[3], TokenKind::Assign);
    assert_eq!(tokens[4], TokenKind::Int(42));
    assert_eq!(tokens[5], TokenKind::Semi);
}

#[test]
fn test_short_var_declaration() {
    let tokens = tokenize("x := 42");
    assert_eq!(tokens.len(), 3);
    assert!(matches!(&tokens[0], TokenKind::Ident(s) if s == "x"));
    assert_eq!(tokens[1], TokenKind::ColonAssign);
    assert_eq!(tokens[2], TokenKind::Int(42));
}

#[test]
fn test_function_call() {
    let tokens = tokenize("foo(1, 2)");
    assert_eq!(tokens.len(), 6);
    assert!(matches!(&tokens[0], TokenKind::Ident(s) if s == "foo"));
    assert_eq!(tokens[1], TokenKind::LParen);
    assert_eq!(tokens[2], TokenKind::Int(1));
    assert_eq!(tokens[3], TokenKind::Comma);
    assert_eq!(tokens[4], TokenKind::Int(2));
    assert_eq!(tokens[5], TokenKind::RParen);
}

#[test]
fn test_struct_literal() {
    let tokens = tokenize(r#"User{name: "Alice"}"#);
    assert_eq!(tokens.len(), 6);
    assert!(matches!(&tokens[0], TokenKind::Ident(s) if s == "User"));
    assert_eq!(tokens[1], TokenKind::LBrace);
    assert!(matches!(&tokens[2], TokenKind::Ident(s) if s == "name"));
    assert_eq!(tokens[3], TokenKind::Colon);
    assert!(matches!(&tokens[4], TokenKind::String(s) if s == "Alice"));
    assert_eq!(tokens[5], TokenKind::RBrace);
}

#[test]
fn test_map_type() {
    let tokens = tokenize("map[string]int");
    assert_eq!(tokens.len(), 5);
    assert_eq!(tokens[0], TokenKind::Map);
    assert_eq!(tokens[1], TokenKind::LBracket);
    assert!(matches!(&tokens[2], TokenKind::Ident(s) if s == "string"));
    assert_eq!(tokens[3], TokenKind::RBracket);
    assert!(matches!(&tokens[4], TokenKind::Ident(s) if s == "int"));
}

#[test]
fn test_slice_type() {
    let tokens = tokenize("[]int");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0], TokenKind::LBracket);
    assert_eq!(tokens[1], TokenKind::RBracket);
    assert!(matches!(&tokens[2], TokenKind::Ident(s) if s == "int"));
}

#[test]
fn test_array_type() {
    let tokens = tokenize("[4]int");
    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[0], TokenKind::LBracket);
    assert_eq!(tokens[1], TokenKind::Int(4));
    assert_eq!(tokens[2], TokenKind::RBracket);
    assert!(matches!(&tokens[3], TokenKind::Ident(s) if s == "int"));
}

#[test]
fn test_binary_expression() {
    let tokens = tokenize("a + b * c");
    assert_eq!(tokens.len(), 5);
    assert!(matches!(&tokens[0], TokenKind::Ident(_)));
    assert_eq!(tokens[1], TokenKind::Plus);
    assert!(matches!(&tokens[2], TokenKind::Ident(_)));
    assert_eq!(tokens[3], TokenKind::Star);
    assert!(matches!(&tokens[4], TokenKind::Ident(_)));
}

#[test]
fn test_comparison_expression() {
    let tokens = tokenize("x >= 0 && x <= 100");
    assert_eq!(tokens.len(), 7);
    assert_eq!(tokens[1], TokenKind::GtEq);
    assert_eq!(tokens[3], TokenKind::And);
    assert_eq!(tokens[5], TokenKind::LtEq);
}

#[test]
fn test_func_declaration() {
    let input = "func main() int { return 0; }";
    let tokens = tokenize(input);
    assert_eq!(tokens.len(), 10);
    assert_eq!(tokens[0], TokenKind::Func);
    assert!(matches!(&tokens[1], TokenKind::Ident(s) if s == "main"));
    assert_eq!(tokens[2], TokenKind::LParen);
    assert_eq!(tokens[3], TokenKind::RParen);
    assert!(matches!(&tokens[4], TokenKind::Ident(s) if s == "int"));
    assert_eq!(tokens[5], TokenKind::LBrace);
    assert_eq!(tokens[6], TokenKind::Return);
    assert_eq!(tokens[7], TokenKind::Int(0));
    assert_eq!(tokens[8], TokenKind::Semi);
    assert_eq!(tokens[9], TokenKind::RBrace);
}

// ═══════════════════════════════════════════════════════════════════════════
// Span Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_token_spans() {
    let input = "var x = 42;";
    let tokens: Vec<_> = Lexer::new(input).collect();

    // "var" at positions 0-3
    assert_eq!(tokens[0].span.start, 0);
    assert_eq!(tokens[0].span.end, 3);

    // "x" at position 4-5
    assert_eq!(tokens[1].span.start, 4);
    assert_eq!(tokens[1].span.end, 5);

    // "=" at position 6-7
    assert_eq!(tokens[2].span.start, 6);
    assert_eq!(tokens[2].span.end, 7);

    // "42" at positions 8-10
    assert_eq!(tokens[3].span.start, 8);
    assert_eq!(tokens[3].span.end, 10);

    // ";" at position 10-11
    assert_eq!(tokens[4].span.start, 10);
    assert_eq!(tokens[4].span.end, 11);
}

// ═══════════════════════════════════════════════════════════════════════════
// Edge Cases
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_consecutive_operators() {
    let tokens = tokenize("++");
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0], TokenKind::Plus);
    assert_eq!(tokens[1], TokenKind::Plus);

    let tokens = tokenize("--");
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0], TokenKind::Minus);
    assert_eq!(tokens[1], TokenKind::Minus);
}

#[test]
fn test_invalid_character() {
    assert_single("@", TokenKind::Invalid('@'));
    assert_single("#", TokenKind::Invalid('#'));
    assert_single("$", TokenKind::Invalid('$'));
}

#[test]
fn test_tokenize_method() {
    let mut lexer = Lexer::new("a b c");
    let tokens = lexer.tokenize();
    assert_eq!(tokens.len(), 3);
}

#[test]
fn test_iterator() {
    let tokens: Vec<_> = Lexer::new("a b c").collect();
    assert_eq!(tokens.len(), 3);
}

// ═══════════════════════════════════════════════════════════════════════════
// Parser Tests
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_parse_empty_file() {
    let file = parser::parse("").unwrap();
    assert!(file.package.is_none());
    assert!(file.imports.is_empty());
    assert!(file.decls.is_empty());
}

#[test]
fn test_parse_package_clause() {
    let file = parser::parse("package main;").unwrap();
    assert!(file.package.is_some());
    assert_eq!(file.package.unwrap().name.name, "main");
}

#[test]
fn test_parse_import() {
    let file = parser::parse(r#"import "fmt";"#).unwrap();
    assert_eq!(file.imports.len(), 1);
    assert_eq!(file.imports[0].path, "fmt");
}

#[test]
fn test_parse_var_decl() {
    let file = parser::parse("var x int;").unwrap();
    assert_eq!(file.decls.len(), 1);
    match &file.decls[0] {
        TopDecl::Var(var) => {
            assert_eq!(var.specs[0].name.name, "x");
            assert!(matches!(&var.specs[0].ty, Some(Type::Named(id)) if id.name == "int"));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_parse_var_with_init() {
    let file = parser::parse("var x = 42;").unwrap();
    match &file.decls[0] {
        TopDecl::Var(var) => {
            assert!(var.specs[0].ty.is_none());
            assert!(matches!(
                &var.specs[0].value,
                Some(Expr::Literal(Literal::Int(42, _)))
            ));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_parse_const_decl() {
    let file = parser::parse("const PI = 3.14;").unwrap();
    match &file.decls[0] {
        TopDecl::Const(c) => {
            assert_eq!(c.specs[0].name.name, "PI");
        }
        _ => panic!("expected const decl"),
    }
}

#[test]
fn test_parse_type_decl_struct() {
    let file = parser::parse("type User struct { name string; };").unwrap();
    match &file.decls[0] {
        TopDecl::Type(t) => {
            assert_eq!(t.name.name, "User");
            if let Type::Struct(s) = &t.ty {
                assert_eq!(s.fields.len(), 1);
                assert_eq!(s.fields[0].name.name, "name");
            } else {
                panic!("expected struct type");
            }
        }
        _ => panic!("expected type decl"),
    }
}

#[test]
fn test_parse_func_decl() {
    let file = parser::parse("func main() { }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            assert_eq!(f.name.name, "main");
            assert!(f.receiver.is_none());
            assert!(f.params.is_empty());
            assert!(f.result.is_none());
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_parse_func_with_params_and_result() {
    let file = parser::parse("func add(a int, b int) int { return a + b; }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            assert_eq!(f.name.name, "add");
            assert_eq!(f.params.len(), 2);
            assert!(f.result.is_some());
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_parse_method() {
    let file = parser::parse("func (u User) Name() string { return u.name; }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            assert!(f.receiver.is_some());
            let r = f.receiver.as_ref().unwrap();
            assert_eq!(r.name.name, "u");
            assert_eq!(r.ty.name, "User");
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_parse_interface() {
    let source = r#"interface Reader {
        Read(buf []byte) int;
    };"#;
    let file = parser::parse(source).unwrap();
    match &file.decls[0] {
        TopDecl::Interface(i) => {
            assert_eq!(i.name.name, "Reader");
            assert_eq!(i.elements.len(), 1);
        }
        _ => panic!("expected interface decl"),
    }
}

#[test]
fn test_parse_implements() {
    let file = parser::parse("implements File : Reader, Writer;").unwrap();
    match &file.decls[0] {
        TopDecl::Implements(i) => {
            assert_eq!(i.type_name.name, "File");
            assert_eq!(i.interfaces.len(), 2);
        }
        _ => panic!("expected implements decl"),
    }
}

#[test]
fn test_parse_if_stmt() {
    let file = parser::parse("func f() { if x > 0 { y = 1; } }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            assert_eq!(f.body.stmts.len(), 1);
            assert!(matches!(&f.body.stmts[0], Stmt::If(_)));
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_if_else() {
    let file = parser::parse("func f() { if x { a = 1; } else { b = 2; } }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::If(i) = &f.body.stmts[0] {
                assert!(i.else_clause.is_some());
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_for_loop() {
    let file = parser::parse("func f() { for i := 0; i < 10; i += 1 { x = i; } }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::For(fo) = &f.body.stmts[0] {
                assert!(fo.init.is_some());
                assert!(fo.cond.is_some());
                assert!(fo.post.is_some());
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_while_loop() {
    let file = parser::parse("func f() { for x > 0 { x -= 1; } }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::For(fo) = &f.body.stmts[0] {
                assert!(fo.init.is_none());
                assert!(fo.cond.is_some());
                assert!(fo.post.is_none());
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_infinite_loop() {
    let file = parser::parse("func f() { for { break; } }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::For(fo) = &f.body.stmts[0] {
                assert!(fo.init.is_none());
                assert!(fo.cond.is_none());
                assert!(fo.post.is_none());
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_switch() {
    let file =
        parser::parse("func f() { switch x { case 1: a = 1; case 2: b = 2; default: c = 3; } }")
            .unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::Switch(s) = &f.body.stmts[0] {
                assert_eq!(s.cases.len(), 2);
                assert!(s.default.is_some());
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_binary_expr() {
    let file = parser::parse("func f() { x = a + b * c; }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::Assign(a) = &f.body.stmts[0] {
                // a + (b * c) due to precedence
                if let Expr::Binary(b) = &a.right[0] {
                    assert!(matches!(b.op, BinaryOp::Add));
                }
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_call_expr() {
    let file = parser::parse("func f() { foo(1, 2, 3); }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::Expr(e) = &f.body.stmts[0] {
                if let Expr::Call(c) = &e.expr {
                    assert_eq!(c.args.len(), 3);
                }
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_index_expr() {
    let file = parser::parse("func f() { x = arr[0]; }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::Assign(a) = &f.body.stmts[0] {
                assert!(matches!(&a.right[0], Expr::Index(_)));
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_selector_expr() {
    let file = parser::parse("func f() { x = user.name; }").unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::Assign(a) = &f.body.stmts[0] {
                if let Expr::Selector(s) = &a.right[0] {
                    assert_eq!(s.field.name, "name");
                }
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_composite_literal() {
    let file = parser::parse(r#"func f() { u = User{name: "Alice"}; }"#).unwrap();
    match &file.decls[0] {
        TopDecl::Func(f) => {
            if let Stmt::Assign(a) = &f.body.stmts[0] {
                if let Expr::CompositeLit(c) = &a.right[0] {
                    assert_eq!(c.elements.len(), 1);
                }
            }
        }
        _ => panic!("expected func"),
    }
}

#[test]
fn test_parse_array_type() {
    let file = parser::parse("var arr [10]int;").unwrap();
    match &file.decls[0] {
        TopDecl::Var(v) => {
            if let Some(Type::Array(a)) = &v.specs[0].ty {
                assert_eq!(a.len, 10);
            }
        }
        _ => panic!("expected var"),
    }
}

#[test]
fn test_parse_slice_type() {
    let file = parser::parse("var s []string;").unwrap();
    match &file.decls[0] {
        TopDecl::Var(v) => {
            assert!(matches!(&v.specs[0].ty, Some(Type::Slice(_))));
        }
        _ => panic!("expected var"),
    }
}

#[test]
fn test_parse_map_type() {
    let file = parser::parse("var m map[string]int;").unwrap();
    match &file.decls[0] {
        TopDecl::Var(v) => {
            assert!(matches!(&v.specs[0].ty, Some(Type::Map(_))));
        }
        _ => panic!("expected var"),
    }
}
