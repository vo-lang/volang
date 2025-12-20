# gox-syntax

Lexer, AST definitions, and parser for the GoX programming language.

## Overview

This crate provides the frontend components of the GoX compiler:

- **Lexer** - Tokenization of GoX source code
- **AST** - Abstract Syntax Tree definitions
- **Parser** - Recursive descent parser producing AST

## Modules

### `token`
Token definitions and token kinds:
- `Token` - A token with kind, span, and optional value
- `TokenKind` - All possible token types (keywords, operators, literals, etc.)
- Automatic semicolon insertion rules

### `lexer`
Tokenization of source code:
- `Lexer` - Converts source text into a stream of tokens
- Handles all GoX literals (integers, floats, strings, runes)
- Unicode identifier support
- Comment handling (line and block comments)

### `ast`
Complete AST definitions for GoX:
- `File` - Top-level source file
- `Decl` - Declarations (var, const, type, func)
- `Stmt` - Statements (if, for, switch, return, etc.)
- `Expr` - Expressions (binary, unary, call, index, etc.)
- `Type` - Type expressions (named, array, slice, map, etc.)

### `parser`
Recursive descent parser:
- `Parser` - Parses tokens into AST
- Error recovery for better diagnostics
- Precedence climbing for expressions

## Usage

```rust
use gox_common::source::SourceMap;
use gox_syntax::{lexer::Lexer, parser::Parser};

// Create source map and add file
let mut source_map = SourceMap::new();
let file_id = source_map.add_file("main.gox", r#"
    package main
    
    func main() int {
        return 0
    }
"#);

// Lex the source
let source = source_map.source(file_id).unwrap();
let lexer = Lexer::new(file_id, source);
let tokens = lexer.collect_tokens();

// Parse into AST
let mut parser = Parser::new(file_id, &tokens);
let file = parser.parse_file();
```

## Token Kinds

The lexer recognizes:

- **Keywords**: `break`, `case`, `chan`, `const`, `continue`, `default`, `defer`, `else`, `fallthrough`, `for`, `func`, `go`, `goto`, `if`, `import`, `interface`, `map`, `object`, `package`, `range`, `return`, `select`, `struct`, `switch`, `type`, `var`
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
