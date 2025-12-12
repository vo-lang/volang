# GoX Parser Implementation Guide

This document provides detailed implementation guidance for the GoX parser. It serves as a reference for developers implementing or maintaining the `gox-syntax` crate.

---

## 1. Architecture Overview

### 1.1 Position in Compiler Pipeline

```
Source Code → Lexer → [Tokens] → Parser → [AST] → Analysis → Codegen
```

The parser consumes tokens from the Lexer and produces an Abstract Syntax Tree (AST).

### 1.2 Parsing Strategy

The parser uses a **hybrid approach**:

| Construct | Strategy | Reason |
|-----------|----------|--------|
| Declarations | Recursive Descent | Clear, keyword-driven |
| Statements | Recursive Descent | Statement-initial keywords |
| Expressions | Pratt Parser | Handles precedence elegantly |
| Types | Recursive Descent | Prefix-determined |

### 1.3 Module Organization

```
crates/gox-syntax/src/parser/
├── mod.rs      # Parser struct, entry point, helpers
├── decl.rs     # TopDecl, VarDecl, TypeDecl, FuncDecl, InterfaceDecl
├── stmt.rs     # Statements, blocks, control flow
├── expr.rs     # Pratt expression parser
└── types.rs    # Type parsing (optional, can be in decl.rs)
```

---

## 2. Parser State

### 2.1 Core Struct

```rust
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    
    // Disambiguation flag
    allow_composite_lit: bool,
    
    // Error collection (if supporting recovery)
    diagnostics: Vec<Diagnostic>,
}
```

### 2.2 Token Management

```rust
impl Parser<'_> {
    /// Advance to next token
    fn next_token(&mut self) {
        self.current_token = std::mem::replace(
            &mut self.peek_token,
            self.lexer.next_token()
        );
    }
    
    /// Check current token kind without consuming
    fn cur_token_is(&self, kind: &TokenKind) -> bool {
        &self.current_token.kind == kind
    }
    
    /// Check peek token kind without consuming
    fn peek_token_is(&self, kind: &TokenKind) -> bool {
        &self.peek_token.kind == kind
    }
    
    /// Expect and consume peek token, error if mismatch
    fn expect_peek(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        if self.peek_token_is(&kind) {
            self.next_token();
            Ok(())
        } else {
            Err(self.error_expected(&kind))
        }
    }
}
```

**Invariant**: After parsing any complete construct, `current_token` should be the last token of that construct (i.e., we've "consumed past" it).

---

## 3. Grammar Productions

All grammar is derived from the Language Specification. Key productions:

### 3.1 File Structure

```ebnf
File ::= PackageClause? ImportDecl* TopDecl* EOF ;
```

```rust
fn parse_file(&mut self) -> Result<File, ParseError> {
    let package = if self.cur_token_is(&TokenKind::Package) {
        Some(self.parse_package_clause()?)
    } else {
        None
    };
    
    let mut imports = vec![];
    while self.cur_token_is(&TokenKind::Import) {
        imports.push(self.parse_import_decl()?);
    }
    
    let mut decls = vec![];
    while !self.cur_token_is(&TokenKind::EOF) {
        decls.push(self.parse_top_decl()?);
    }
    
    Ok(File { package, imports, decls })
}
```

### 3.2 Top-Level Declarations

```ebnf
TopDecl ::= VarDecl | ConstDecl | TypeDecl | InterfaceDecl | ImplementsDecl | FuncDecl ;
```

**Dispatch by keyword**:

| `current_token` | Parse Function |
|-----------------|----------------|
| `var` | `parse_var_decl()` |
| `const` | `parse_const_decl()` |
| `type` | `parse_type_decl()` |
| `interface` | `parse_interface_decl()` |
| `implements` | `parse_implements_decl()` |
| `func` | `parse_func_decl()` |

### 3.3 Type Parsing

```ebnf
Type ::= Ident | ArrayType | SliceType | MapType | FuncType | StructType ;
```

**Dispatch by prefix**:

| Prefix | Type Form |
|--------|-----------|
| `Ident` | Named type (e.g., `int`, `User`) |
| `[` + `IntLit` + `]` | Array (e.g., `[4]int`) |
| `[` + `]` | Slice (e.g., `[]int`) |
| `map` | Map (e.g., `map[string]int`) |
| `func` | Function type |
| `struct` | Inline struct |

```rust
fn parse_type(&mut self) -> Result<Type, ParseError> {
    match &self.current_token.kind {
        TokenKind::Ident(_) => self.parse_named_type(),
        TokenKind::LBracket => {
            self.next_token(); // consume [
            if self.cur_token_is(&TokenKind::RBracket) {
                self.parse_slice_type()
            } else {
                self.parse_array_type()
            }
        }
        TokenKind::Map => self.parse_map_type(),
        TokenKind::Func => self.parse_func_type(),
        TokenKind::Struct => self.parse_struct_type(),
        _ => Err(self.error("expected type")),
    }
}
```

---

## 4. Expression Parsing (Pratt)

### 4.1 Precedence Levels

```rust
#[derive(PartialEq, PartialOrd, Clone, Copy)]
enum Precedence {
    Lowest = 0,
    Or,          // ||
    And,         // &&
    Equals,      // == !=
    LessGreater, // < <= > >=
    Sum,         // + -
    Product,     // * / %
    Prefix,      // -x !x
    Call,        // f(x)
    Index,       // a[i] a.b
}
```

### 4.2 Core Loop

```rust
fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
    // Parse prefix (left-hand side)
    let mut left = self.parse_prefix()?;
    
    // Loop: while peek has higher precedence, parse infix
    while !self.peek_token_is(&TokenKind::Semi) 
          && precedence < self.peek_precedence() 
    {
        // Check if peek can start an infix expression
        if !self.has_infix_parse_fn() {
            return Ok(left);
        }
        
        self.next_token();
        left = self.parse_infix(left)?;
    }
    
    Ok(left)
}
```

### 4.3 Prefix Expressions

| Token | Parse Action |
|-------|--------------|
| `Ident` | Identifier |
| `IntLit`, `FloatLit`, `StringLit` | Literal |
| `true`, `false` | Boolean literal |
| `nil` | Nil literal |
| `(` | Grouped expression |
| `-`, `!`, `+` | Unary operator |
| `Type` + `{` | Composite literal (if `allow_composite_lit`) |

### 4.4 Infix Expressions

| Token | Precedence | Parse Action |
|-------|------------|--------------|
| `\|\|` | Or | Binary |
| `&&` | And | Binary |
| `==`, `!=` | Equals | Binary |
| `<`, `<=`, `>`, `>=` | LessGreater | Binary |
| `+`, `-` | Sum | Binary |
| `*`, `/`, `%` | Product | Binary |
| `(` | Call | Function call |
| `[` | Index | Index expression |
| `.` | Index | Selector |
| `{` | Call | Composite literal (if allowed) |

---

## 5. Statement Parsing

### 5.1 Statement Dispatch

```ebnf
Stmt ::= Block | VarDecl | ConstDecl | ShortVarDecl | Assignment 
       | ExprStmt | ReturnStmt | IfStmt | ForStmt | SwitchStmt 
       | BreakStmt | ContinueStmt | EmptyStmt ;
```

**Dispatch logic**:

```rust
fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
    match &self.current_token.kind {
        TokenKind::LBrace => self.parse_block(),
        TokenKind::Var => self.parse_var_decl(),
        TokenKind::Const => self.parse_const_decl(),
        TokenKind::Return => self.parse_return_stmt(),
        TokenKind::If => self.parse_if_stmt(),
        TokenKind::For => self.parse_for_stmt(),
        TokenKind::Switch => self.parse_switch_stmt(),
        TokenKind::Break => self.parse_break_stmt(),
        TokenKind::Continue => self.parse_continue_stmt(),
        TokenKind::Semi => { self.next_token(); Ok(Stmt::Empty) }
        _ => self.parse_simple_stmt(), // Expr, Assignment, ShortVarDecl
    }
}
```

### 5.2 Simple Statement Ambiguity

`SimpleStmt` can be `ExprStmt`, `Assignment`, or `ShortVarDecl`. The parser must look ahead:

```
x           → could be start of any
x := 1      → ShortVarDecl (look for `:=`)
x = 1       → Assignment (look for `=`, `+=`, etc.)
x + 1       → ExprStmt (no assignment op follows)
x()         → ExprStmt (function call)
```

**Strategy**: Parse left side as expression, then check `peek_token`:

```rust
fn parse_simple_stmt(&mut self) -> Result<Stmt, ParseError> {
    let left = self.parse_expr_list()?;
    
    match &self.current_token.kind {
        TokenKind::ColonAssign => {
            // ShortVarDecl: left must be IdentList
            self.parse_short_var_decl(left)
        }
        TokenKind::Assign | TokenKind::PlusAssign | ... => {
            // Assignment
            self.parse_assignment(left)
        }
        _ => {
            // ExprStmt (expect semicolon)
            Ok(Stmt::Expr(left))
        }
    }
}
```

### 5.3 For Statement Forms

```ebnf
ForClause ::= Expr | ForThreeClause ;
ForThreeClause ::= SimpleStmt? ";" Expr? ";" SimpleStmt? ;
```

**Disambiguation**: Presence of `;` determines the form.

```rust
fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
    self.next_token(); // consume 'for'
    
    // Save composite literal flag, disable during condition
    let saved = self.allow_composite_lit;
    self.allow_composite_lit = false;
    
    // Try to detect form
    if self.cur_token_is(&TokenKind::LBrace) {
        // for { ... } - infinite loop
        self.allow_composite_lit = saved;
        let body = self.parse_block()?;
        return Ok(Stmt::For { init: None, cond: None, post: None, body });
    }
    
    // Parse first part
    let first = self.parse_simple_stmt_or_expr()?;
    
    if self.cur_token_is(&TokenKind::LBrace) {
        // for cond { ... } - while form
        self.allow_composite_lit = saved;
        let body = self.parse_block()?;
        return Ok(Stmt::For { init: None, cond: Some(first), post: None, body });
    }
    
    // Must be three-clause form: init; cond; post
    self.expect(&TokenKind::Semi)?;
    let cond = if !self.cur_token_is(&TokenKind::Semi) {
        Some(self.parse_expr(Precedence::Lowest)?)
    } else { None };
    self.expect(&TokenKind::Semi)?;
    let post = if !self.cur_token_is(&TokenKind::LBrace) {
        Some(self.parse_simple_stmt()?)
    } else { None };
    
    self.allow_composite_lit = saved;
    let body = self.parse_block()?;
    
    Ok(Stmt::For { init: Some(first), cond, post, body })
}
```

---

## 6. Disambiguation

### 6.1 Composite Literal vs Block

**Problem**: `{` after a type looks like both:
- Composite literal: `User{name: "x"}`
- Block start: `if cond { ... }`

**Solution**: The `allow_composite_lit` flag.

| Context | `allow_composite_lit` | `{` means |
|---------|----------------------|-----------|
| Top-level expression | `true` | Composite literal |
| `if` condition | `false` | Block |
| `for` condition | `false` | Block |
| `switch` expression | `false` | Block |
| Inside `(...)` | `true` | Composite literal |
| Inside block | `true` | Nested block or literal |

### 6.2 Type vs Expression

**Problem**: `User` could be:
- A type (in `var x User`)
- An expression (in `x := User{...}`)

**Solution**: Context determines interpretation. In `parse_type()`, we expect a type. In `parse_expr()`, we treat identifiers as expressions, and composite literal parsing handles `Type{...}`.

### 6.3 `:=` Always Declares

> **GoX-specific**: Unlike Go, `:=` always declares new variables. It never reuses existing variables.

This simplifies parsing: no need to check if identifiers already exist.

---

## 7. Error Handling

### 7.1 Integration with `gox-common`

```rust
use gox_common::diagnostic::{Diagnostic, Severity};
use gox_common::source::FileId;
use gox_common::span::Span;

impl Parser<'_> {
    fn error(&self, msg: &str) -> ParseError {
        ParseError {
            diagnostic: Diagnostic::error(msg)
                .with_label(self.current_token.span, self.file_id, "here"),
        }
    }
    
    fn error_expected(&self, expected: &TokenKind) -> ParseError {
        self.error(&format!("expected {:?}, found {:?}", expected, self.current_token.kind))
    }
}
```

### 7.2 Recovery Strategy (Future)

For V1, we stop at first error. Future recovery:

1. **Panic mode**: Skip tokens until a synchronizing token (`;`, `}`, keyword)
2. **Continue parsing** from sync point
3. **Collect all diagnostics**

Sync tokens:
- Statement level: `;`, `}`
- Declaration level: `func`, `type`, `var`, `const`, `interface`, `implements`

---

## 8. AST Node Reference

Key AST types (defined in `ast.rs`):

```rust
pub struct File {
    pub package: Option<PackageClause>,
    pub imports: Vec<ImportDecl>,
    pub decls: Vec<TopDecl>,
}

pub enum TopDecl {
    Var(VarDecl),
    Const(ConstDecl),
    Type(TypeDecl),
    Interface(InterfaceDecl),
    Implements(ImplementsDecl),
    Func(FuncDecl),
}

pub enum Stmt {
    Block(Block),
    Var(VarDecl),
    Const(ConstDecl),
    ShortVar(ShortVarDecl),
    Assign(Assignment),
    Expr(Expr),
    Return(ReturnStmt),
    If(IfStmt),
    For(ForStmt),
    Switch(SwitchStmt),
    Break,
    Continue,
    Empty,
}

pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
    Index(IndexExpr),
    Selector(SelectorExpr),
    CompositeLit(CompositeLit),
}

pub enum Type {
    Named(Ident),
    Array(ArrayType),
    Slice(SliceType),
    Map(MapType),
    Func(FuncType),
    Struct(StructType),
}
```

---

## 9. Testing

### 9.1 Data-Driven Tests

Place `.gox` files in `src/test_data/`:

```
src/test_data/
├── expr_binary.gox
├── stmt_if.gox
├── decl_func.gox
└── ...
```

Use `insta::glob!` to run all:

```rust
#[test]
fn test_parser() {
    insta::glob!("test_data/*.gox", |path| {
        let input = std::fs::read_to_string(path).unwrap();
        let result = parse(&input);
        insta::assert_debug_snapshot!(result);
    });
}
```

### 9.2 Error Case Tests

Test that invalid syntax produces correct errors:

```gox
// test_data/error_missing_semi.gox
var x int  // missing semicolon
```

Expected: `ParseError { message: "expected ';'" }`

---

## 10. Implementation Checklist

### Phase 1: Foundation
- [ ] `Parser` struct with token management
- [ ] `parse_file()` entry point
- [ ] Basic error reporting

### Phase 2: Types
- [ ] `parse_type()` dispatcher
- [ ] Named types, arrays, slices
- [ ] Map types, function types
- [ ] Struct types with field tags

### Phase 3: Expressions
- [ ] Precedence enum
- [ ] `parse_expr()` Pratt loop
- [ ] Binary, unary, call, index, selector
- [ ] Composite literals with `allow_composite_lit`

### Phase 4: Statements
- [ ] `parse_stmt()` dispatcher
- [ ] Block, return, break, continue
- [ ] If, for, switch
- [ ] Assignment, ShortVarDecl

### Phase 5: Declarations
- [ ] VarDecl, ConstDecl, TypeDecl
- [ ] FuncDecl with receiver
- [ ] InterfaceDecl with embedding
- [ ] ImplementsDecl

### Phase 6: Polish
- [ ] Semicolon handling
- [ ] Comprehensive error messages
- [ ] Span tracking for all nodes
- [ ] Test coverage

