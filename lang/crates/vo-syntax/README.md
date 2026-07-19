# vo-syntax

Lexer, AST definitions, parser, and source formatter for the Vo programming language.

## Overview

This crate provides the frontend components of the Vo compiler:

- **Lexer** - Tokenization of Vo source code
- **AST** - Abstract Syntax Tree definitions
- **Parser** - Recursive descent parser producing AST
- **Formatter** - Parse-preserving, idempotent source formatting

## Modules

### `token`
Token definitions and token kinds:
- `Token` - A token with kind, span, and optional value
- `TokenKind` - All possible token types (keywords, operators, literals, etc.)
- Automatic semicolon insertion rules

### `lexer`
Tokenization of source code:
- `Lexer` - Converts source text into a stream of tokens
- Handles all Vo literals (integers, floats, strings, runes)
- Unicode identifier support
- Comment handling (line and block comments)

### `ast`
Complete AST definitions for Vo:
- `File` - Top-level source file
- `Decl` - Declarations (var, const, type, func)
- `Stmt` - Statements (if, for, switch, return, etc.)
- `Expr` - Expressions (binary, unary, call, index, etc.)
- `Type` - Type expressions (named, array, slice, map, etc.)

### `parser`
Recursive descent parser:
- `Parser` - Parses tokens into AST
- Error recovery for better diagnostics
- Iterative Pratt parsing for binary-expression spines

### `formatter`
Canonical source formatting:
- Preserves comments and inline module metadata
- Rejects parse errors and semantic token drift
- Verifies idempotence before returning output

## Usage

```rust
use vo_syntax::Lexer;

let source = r#"package main

func main() int {
    return 0
}
"#;

// Lex the source
let (tokens, lexer_diagnostics) = Lexer::new(source, 0).collect_tokens();
assert!(!lexer_diagnostics.has_errors());

// Parse directly from source. The parser owns its streaming lexer.
let (file, parser_diagnostics, interner) = vo_syntax::parse(source, 0);
assert!(!parser_diagnostics.has_errors());
assert_eq!(tokens.last().unwrap().kind, vo_syntax::TokenKind::Eof);
assert_eq!(file.package.unwrap().as_str(&interner), Some("main"));

let formatted = vo_syntax::format_source("package main\nfunc main(){}\n").unwrap();
assert_eq!(vo_syntax::format_source(&formatted).unwrap(), formatted);
```

## Token Kinds

The lexer recognizes:

- **Keywords**: `break`, `case`, `chan`, `const`, `continue`, `default`, `defer`, `else`, `errdefer`, `fail`, `fallthrough`, `for`, `func`, `go`, `goto`, `if`, `import`, `interface`, `island`, `map`, `package`, `port`, `range`, `return`, `select`, `struct`, `switch`, `type`, `var`
- **Literals**: integers (decimal, hex, octal, binary), floats, strings, runes
- **Operators**: arithmetic, bitwise, comparison, logical, assignment
- **Delimiters**: parentheses, brackets, braces, commas, semicolons

## AST Structure

```
File
├── package: Option<Ident>
├── imports: Vec<ImportDecl>
└── decls: Vec<Decl>
    ├── VarDecl
    ├── ConstDecl
    ├── TypeDecl (includes interface types)
    └── FuncDecl

Stmt
├── Block
├── VarDecl / ConstDecl
├── ExprStmt
├── Assignment
├── If / For / Switch / Select
├── Return / Break / Continue / Goto
├── Go / Defer
├── Send
└── Labeled / Empty

Expr
├── Ident / Literal
├── Binary / Unary
├── Call / Index / Slice
├── Selector / TypeAssertion
├── CompositeLit / FuncLit
└── Receive
```

## Error Recovery

The parser implements error recovery to continue parsing after errors:
- Synchronizes at statement boundaries
- Reports multiple errors per compilation
- Provides helpful error messages with source locations

## Structural Resource Limits

One UTF-8 source file is limited to 16 MiB and reports `E1107` when the source
size or global span range cannot be represented. Recursive syntax nesting is
limited to 128 parser frames and reports `E1106`.
Flat left-associative binary expressions are built iteratively, so they use a
separate limit of 512 binary operators on any AST root-to-leaf path. Exceeding
that structural limit reports `E1108`. The path-wide count includes operators
separated by precedence, parentheses, calls, selectors, conversions, types, or
function bodies, which keeps every downstream recursive AST consumer bounded.
Each file retains at most 256 concrete syntax diagnostics; `E1109` records that
later diagnostics were suppressed while parsing still completes deterministically.
