# GoX Parser Implementation Guide

This document provides detailed implementation guidance for the GoX parser, based on the [Language Specification](./language_spec.md).

> **Naming Convention**: In the Rust codebase, the GoX `object` type is internally named **`obx`** (e.g., `TokenKind::Obx`, `Type::Obx`) to avoid confusion with Rust's `Object` trait and other common naming patterns. When reading the source code, remember that `obx` = GoX `object`.

---

## 1. Architecture Overview

### 1.1 Compiler Pipeline

```
Source Code → Lexer → [Tokens] → Parser → [AST] → Analysis → Codegen
```

The parser consumes tokens from the Lexer and produces an Abstract Syntax Tree (AST).

### 1.2 Parsing Strategy

| Construct | Strategy | Reason |
|-----------|----------|--------|
| Declarations | Recursive Descent | Clear, keyword-driven |
| Statements | Recursive Descent | Statement-initial keywords |
| Expressions | Pratt Parser | Handles precedence elegantly |
| Types | Recursive Descent | Prefix-determined |

### 1.3 Module Organization

```
crates/gox-syntax/src/
├── lexer.rs    # Lexer with automatic semicolon insertion
├── token.rs    # Token and TokenKind definitions
├── ast.rs      # AST node definitions
└── parser/
    ├── mod.rs      # Parser struct, entry point, helpers
    ├── decl.rs     # Declarations and types
    ├── stmt.rs     # Statements and control flow
    └── expr.rs     # Pratt expression parser
```

---

## 2. Lexer Integration

### 2.1 Automatic Semicolon Insertion

The lexer automatically inserts a semicolon after a line's final token if that token is:
- An identifier or basic literal (`IntLit`, `FloatLit`, `RuneLit`, `StringLit`)
- One of the keywords `break`, `continue`, `fallthrough`, `return`
- One of the operators `++`, `--`
- A closing delimiter: `)`, `]`, `}`

This matches Go's semicolon insertion rules.

### 2.2 Token Kinds

Key token categories:

| Category | Examples |
|----------|----------|
| Literals | `IntLit`, `FloatLit`, `RuneLit`, `StringLit` |
| Identifiers | `Ident` |
| Keywords | `Func`, `Var`, `Const`, `Type`, `Interface`, `Struct`, `Obx`, `Map`, `Chan`, ... |
| Operators | `Plus`, `Minus`, `Star`, `Slash`, `Eq`, `ColonAssign`, `Arrow`, ... |
| Delimiters | `LParen`, `RParen`, `LBracket`, `RBracket`, `LBrace`, `RBrace` |

---

## 3. Parser State

### 3.1 Core Struct

```rust
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    peek: Token,
    
    // Disambiguation: disables composite literal parsing in conditions
    allow_composite_lit: bool,
}
```

### 3.2 Token Management

```rust
impl Parser<'_> {
    fn next_token(&mut self) {
        self.current = std::mem::replace(&mut self.peek, self.lexer.next_token());
    }
    
    fn cur_is(&self, kind: &TokenKind) -> bool {
        &self.current.kind == kind
    }
    
    fn peek_is(&self, kind: &TokenKind) -> bool {
        &self.peek.kind == kind
    }
    
    fn expect(&mut self, kind: &TokenKind) -> ParseResult<Span> {
        if self.cur_is(kind) {
            let span = self.current.span;
            self.next_token();
            Ok(span)
        } else {
            Err(self.error(&format!("expected {:?}", kind)))
        }
    }
    
    fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.cur_is(kind) {
            self.next_token();
            true
        } else {
            false
        }
    }
}
```

**Invariant**: After parsing any complete construct, `current` points to the next token to be parsed.

---

## 4. Grammar Productions

### 4.1 File Structure (§4 of Spec)

```ebnf
File ::= PackageClause? ImportDecl* TopDecl* EOF ;
PackageClause ::= "package" Ident ";" ;
ImportDecl    ::= "import" StringLit ";" ;
```

```rust
fn parse_file(&mut self) -> ParseResult<SourceFile> {
    let package = if self.cur_is(&TokenKind::Package) {
        self.next_token();
        let name = self.parse_ident()?;
        self.expect(&TokenKind::Semi)?;
        Some(name)
    } else {
        None
    };
    
    let mut imports = Vec::new();
    while self.cur_is(&TokenKind::Import) {
        imports.push(self.parse_import_decl()?);
    }
    
    let mut decls = Vec::new();
    while !self.at_eof() {
        if self.eat(&TokenKind::Semi) { continue; }
        decls.push(self.parse_top_decl()?);
    }
    
    Ok(SourceFile { package, imports, decls, span })
}
```

### 4.2 Top-Level Declarations (§4.2, §5, §7)

```ebnf
TopDecl ::= VarDecl | ConstDecl | TypeDecl | InterfaceDecl | FuncDecl ;
```

**Dispatch by keyword**:

| Token | Parse Function |
|-------|----------------|
| `var` | `parse_var_decl()` |
| `const` | `parse_const_decl()` |
| `type` | `parse_type_decl()` |
| `interface` | `parse_interface_decl()` |
| `func` | `parse_func_decl()` |

### 4.3 Variable Declarations (§5.1)

```ebnf
VarDecl     ::= "var" ( VarSpec ";" | "(" VarSpecList ")" ";" ) ;
VarSpecList ::= ( VarSpec ";" )* ;
VarSpec     ::= IdentList Type? ( "=" ExprList )? ;
```

Supports both single and grouped declarations:
```gox
var x int
var (
    a = 1
    b, c = 2, 3
)
```

### 4.4 Constant Declarations (§5.2)

```ebnf
ConstDecl     ::= "const" ( ConstSpec ";" | "(" ConstSpecList ")" ";" ) ;
ConstSpecList ::= ( ConstSpec ";" )* ;
ConstSpec     ::= IdentList Type? ( "=" ExprList )? ;
```

**iota handling**: Track the spec index within a const block; `iota` increments for each spec.

### 4.5 Type Declarations (§5.5)

```ebnf
TypeDecl ::= "type" Ident Type ";" ;
```

### 4.6 Interface Declarations (§7.1)

```ebnf
InterfaceDecl ::= "interface" Ident "{" InterfaceElem* "}" ";" ;
InterfaceElem ::= MethodSpec | EmbeddedIface ;
MethodSpec    ::= Ident "(" ParamList? ")" ResultType? ";" ;
EmbeddedIface ::= Ident ";" ;
```

### 4.7 Function Declarations (§7.4)

```ebnf
FuncDecl ::= "func" Receiver? Ident "(" ParamList? ")" ResultType? Block ;
Receiver ::= "(" Ident Ident ")" ;
ParamList ::= Param ( "," Param )* ;
Param ::= IdentList Type ;
```

**Variadic**: Final param may be `Ident "..." Type`.

---

## 5. Type Parsing (§6)

```ebnf
Type ::= Ident
       | ArrayType | SliceType | MapType | ChanType
       | FuncType | StructType | ObjectType | InterfaceType ;
```

**Dispatch by prefix**:

| Prefix | Type Form | Internal Name |
|--------|-----------|---------------|
| `Ident` | Named type | `Type::Named` |
| `[` + `IntLit` + `]` | Array | `Type::Array` |
| `[` + `]` | Slice | `Type::Slice` |
| `map` | Map | `Type::Map` |
| `chan`, `<-chan`, `chan<-` | Channel | `Type::Chan` |
| `func` | Function type | `Type::Func` |
| `struct` | Struct | `Type::Struct` |
| `object` | Object | `Type::Obx` |
| `interface` | Interface | `Type::Interface` |

```rust
fn parse_type(&mut self) -> ParseResult<Type> {
    match &self.current.kind {
        TokenKind::Ident(_) => {
            let id = self.parse_ident()?;
            Ok(Type::Named(id))
        }
        TokenKind::LBracket => {
            self.next_token();
            if self.cur_is(&TokenKind::RBracket) {
                self.parse_slice_type()
            } else {
                self.parse_array_type()
            }
        }
        TokenKind::Map => self.parse_map_type(),
        TokenKind::Chan => self.parse_chan_type(),
        TokenKind::Arrow => self.parse_recv_chan_type(),  // <-chan
        TokenKind::Func => self.parse_func_type(),
        TokenKind::Struct => self.parse_struct_type(),
        TokenKind::Obx => self.parse_obx_type(),
        TokenKind::Interface => self.parse_interface_type(),
        _ => Err(self.error("expected type")),
    }
}
```

### 5.1 Channel Direction (§6.6)

```ebnf
ChanType ::= ( "chan" | "chan" "<-" | "<-" "chan" ) Type ;
```

Three forms:
- `chan T` — bidirectional
- `chan<- T` — send-only (check for `<-` after `chan`)
- `<-chan T` — receive-only (starts with `<-`)

### 5.2 Struct and Object Types (§6.9, §6.10)

```ebnf
StructType ::= "struct" "{" FieldDecl* "}" ;
ObjectType ::= "object" "{" FieldDecl* "}" ;
FieldDecl  ::= ( IdentList Type | Type ) Tag? ";" ;
Tag        ::= StringLit ;
```

**Anonymous fields**: A field can be just a type name (embedding).

---

## 6. Statement Parsing (§8)

```ebnf
Stmt ::= Block | VarDecl | ConstDecl | ShortVarDecl | Assignment
       | ExprStmt | ReturnStmt | IfStmt | ForStmt | SwitchStmt
       | SelectStmt | GoStmt | DeferStmt | SendStmt
       | BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt
       | LabeledStmt | IncDecStmt | EmptyStmt ;
```

**Dispatch logic**:

```rust
fn parse_stmt(&mut self) -> ParseResult<Stmt> {
    match &self.current.kind {
        TokenKind::LBrace => self.parse_block(),
        TokenKind::Var => self.parse_var_decl(),
        TokenKind::Const => self.parse_const_decl(),
        TokenKind::Return => self.parse_return_stmt(),
        TokenKind::If => self.parse_if_stmt(),
        TokenKind::For => self.parse_for_stmt(),
        TokenKind::Switch => self.parse_switch_stmt(),
        TokenKind::Select => self.parse_select_stmt(),
        TokenKind::Go => self.parse_go_stmt(),
        TokenKind::Defer => self.parse_defer_stmt(),
        TokenKind::Break => self.parse_break_stmt(),
        TokenKind::Continue => self.parse_continue_stmt(),
        TokenKind::Goto => self.parse_goto_stmt(),
        TokenKind::Fallthrough => self.parse_fallthrough_stmt(),
        TokenKind::Semi => { self.next_token(); Ok(Stmt::Empty(span)) }
        _ => self.parse_simple_stmt(),
    }
}
```

### 6.1 Simple Statement Disambiguation

`SimpleStmt` can be `ExprStmt`, `Assignment`, `ShortVarDecl`, `IncDecStmt`, or `SendStmt`.

**Strategy**: Parse left side as expression list, then check next token:

```rust
fn parse_simple_stmt(&mut self) -> ParseResult<Stmt> {
    let left = self.parse_expr_list()?;
    
    match &self.current.kind {
        TokenKind::ColonAssign => self.parse_short_var_decl(left),
        TokenKind::Assign | TokenKind::PlusAssign | ... => self.parse_assignment(left),
        TokenKind::PlusPlus | TokenKind::MinusMinus => self.parse_inc_dec(left),
        TokenKind::Arrow => self.parse_send_stmt(left),  // ch <- value
        _ => Ok(Stmt::Expr(left)),
    }
}
```

### 6.2 If Statement (§8.5)

```ebnf
IfStmt ::= "if" ( SimpleStmt ";" )? Expr Block ( "else" ( IfStmt | Block ) )? ;
```

**Disambiguation**: If there's a `;` after the first statement, it's an init statement.

```rust
fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
    self.next_token(); // consume 'if'
    
    let saved = self.allow_composite_lit;
    self.allow_composite_lit = false;
    
    let first = self.parse_simple_stmt_or_expr()?;
    
    let (init, cond) = if self.cur_is(&TokenKind::Semi) {
        self.next_token();
        let cond = self.parse_expr()?;
        (Some(first), cond)
    } else {
        (None, first)
    };
    
    self.allow_composite_lit = saved;
    let body = self.parse_block()?;
    
    let else_branch = if self.cur_is(&TokenKind::Else) {
        self.next_token();
        if self.cur_is(&TokenKind::If) {
            Some(self.parse_if_stmt()?)
        } else {
            Some(self.parse_block()?)
        }
    } else {
        None
    };
    
    Ok(Stmt::If { init, cond, body, else_branch })
}
```

### 6.3 For Statement (§8.6)

```ebnf
ForStmt        ::= "for" ForClause Block ;
ForClause      ::= Expr | ForThreeClause | ForRangeClause ;
ForThreeClause ::= SimpleStmt? ";" Expr? ";" SimpleStmt? ;
ForRangeClause ::= ( IdentList ( ":=" | "=" ) )? "range" Expr ;
```

**Disambiguation**:
- If `{` immediately follows `for` → infinite loop
- If `range` keyword appears → for-range
- If `;` appears → three-clause form
- Otherwise → while-style (condition only)

### 6.4 Switch Statement (§8.7)

```ebnf
SwitchStmt    ::= "switch" ( SimpleStmt ";" )? Expr? "{" CaseClause* DefaultClause? "}" ;
CaseClause    ::= "case" ExprList ":" Stmt* ;
DefaultClause ::= "default" ":" Stmt* ;
```

### 6.5 Type Switch (§11)

```ebnf
TypeSwitchStmt ::= "switch" ( Ident ":=" )? Expr "." "(" "type" ")" "{" TypeCaseClause* "}" ;
TypeCaseClause ::= "case" TypeList ":" Stmt* | "default" ":" Stmt* ;
```

**Detection**: Look for `.(type)` pattern in the switch expression.

### 6.6 Select Statement (§8.8)

```ebnf
SelectStmt ::= "select" "{" SelectCase* "}" ;
SelectCase ::= ( "case" ( SendStmt | RecvStmt ) | "default" ) ":" Stmt* ;
RecvStmt   ::= ( IdentList ( ":=" | "=" ) )? "<-" Expr ;
```

---

## 7. Expression Parsing (Pratt) (§9)

### 7.1 Precedence Levels

From spec §9.1, lowest to highest:

```rust
enum Precedence {
    Lowest = 0,
    Or,          // ||
    And,         // &&
    Equals,      // == !=
    Compare,     // < <= > >=
    Shift,       // << >>
    Sum,         // + - | ^
    Product,     // * / % & &^
    Prefix,      // -x !x ^x
    Postfix,     // f(x) a[i] a.b x.(T)
}
```

### 7.2 Pratt Parser Core

```rust
fn parse_expr(&mut self, precedence: Precedence) -> ParseResult<Expr> {
    let mut left = self.parse_prefix()?;
    
    while precedence < self.peek_precedence() {
        if !self.has_infix_parse_fn() {
            return Ok(left);
        }
        self.next_token();
        left = self.parse_infix(left)?;
    }
    
    Ok(left)
}
```

### 7.3 Prefix Expressions

| Token | Parse Action |
|-------|--------------|
| `Ident` | Identifier |
| `IntLit`, `FloatLit`, `RuneLit`, `StringLit` | Literal |
| `true`, `false` | Boolean (predeclared identifier) |
| `nil` | Nil (predeclared identifier) |
| `(` | Grouped expression |
| `-`, `!`, `+`, `^` | Unary operator |
| `func` | Function literal |
| `<-` | Receive expression |
| Type + `{` | Composite literal (if `allow_composite_lit`) |

### 7.4 Infix/Postfix Expressions

| Token | Precedence | Parse Action |
|-------|------------|--------------|
| `\|\|` | Or | Binary |
| `&&` | And | Binary |
| `==`, `!=` | Equals | Binary |
| `<`, `<=`, `>`, `>=` | Compare | Binary |
| `<<`, `>>` | Shift | Binary |
| `+`, `-`, `\|`, `^` | Sum | Binary |
| `*`, `/`, `%`, `&`, `&^` | Product | Binary |
| `(` | Postfix | Function call |
| `[` | Postfix | Index or slice |
| `.` | Postfix | Selector or type assertion |
| `{` | Postfix | Composite literal (if allowed) |

### 7.5 Composite Literals (§9.4)

```ebnf
CompositeLit ::= Type "{" ElementList? "}" ;
ElementList  ::= Element ( "," Element )* ;
Element      ::= ( Key ":" )? Value ;
```

**Key interpretation**:
- For struct/object: `Key` is field name (Ident)
- For map: `Key` is expression
- For array/slice: `Key` is optional index

### 7.6 Type Assertions (§9.6)

```ebnf
TypeAssertion ::= Expr "." "(" Type ")" ;
```

After parsing `.`, check for `(`. If next token after `(` is `type`, it's a type switch guard.

---

## 8. Disambiguation

### 8.1 Composite Literal vs Block

**Problem**: `{` after a type looks like both composite literal and block start.

**Solution**: The `allow_composite_lit` flag.

| Context | Flag | `{` means |
|---------|------|-----------|
| Top-level expression | `true` | Composite literal |
| `if`/`for`/`switch` condition | `false` | Block |
| Inside `(...)` | `true` | Composite literal |
| Inside block | `true` | Nested block or literal |

### 8.2 Type vs Expression

**Problem**: `User` could be a type or an expression.

**Solution**: Context determines interpretation:
- In `parse_type()`: expect type
- In `parse_expr()`: treat as identifier, composite literal parsing handles `Type{...}`

### 8.3 Channel Direction vs Receive

**Problem**: `<-` can be receive operator or channel direction.

**Solution**:
- In type context: `<-chan` is receive-only channel
- In expression context: `<-expr` is receive expression

---

## 9. AST Node Reference

Key AST types:

```rust
pub struct SourceFile {
    pub package: Option<Ident>,
    pub imports: Vec<ImportDecl>,
    pub decls: Vec<TopDecl>,
    pub span: Span,
}

pub enum TopDecl {
    Var(VarDecl),
    Const(ConstDecl),
    Type(TypeDecl),
    Interface(InterfaceDecl),
    Func(FuncDecl),
}

pub enum Stmt {
    Block(Block),
    Var(VarDecl),
    Const(ConstDecl),
    ShortVar(ShortVarDecl),
    Assign(Assignment),
    Expr(ExprStmt),
    Return(ReturnStmt),
    If(Box<IfStmt>),
    For(Box<ForStmt>),
    ForRange(Box<ForRangeStmt>),
    Switch(Box<SwitchStmt>),
    TypeSwitch(Box<TypeSwitchStmt>),
    Select(Box<SelectStmt>),
    Go(GoStmt),
    Defer(DeferStmt),
    Send(SendStmt),
    Goto(GotoStmt),
    Labeled(Box<LabeledStmt>),
    Fallthrough(Span),
    Break(BreakStmt),
    Continue(ContinueStmt),
    IncDec(IncDecStmt),
    Empty(Span),
}

pub enum Expr {
    Ident(Ident),
    Int(i64, Span),
    Float(f64, Span),
    Rune(char, Span),
    String(String, Span),
    Bool(bool, Span),
    Nil(Span),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Call(Box<CallExpr>),
    Index(Box<IndexExpr>),
    Slice(Box<SliceExpr>),
    Selector(Box<SelectorExpr>),
    TypeAssert(Box<TypeAssertExpr>),
    CompositeLit(Box<CompositeLit>),
    FuncLit(Box<FuncLit>),
    Paren(Box<ParenExpr>),
    Recv(Box<RecvExpr>),
}

pub enum Type {
    Named(Ident),
    Array(Box<ArrayType>),
    Slice(Box<SliceType>),
    Map(Box<MapType>),
    Chan(Box<ChanType>),
    Func(Box<FuncType>),
    Struct(Box<StructType>),
    Obx(Box<ObxType>),      // GoX object type
    Interface(Box<InterfaceType>),
}
```

---

## 10. Error Handling

### 10.1 Error Construction

```rust
impl Parser<'_> {
    fn error(&self, msg: &str) -> ParseError {
        ParseError {
            message: msg.to_string(),
            span: self.current.span,
        }
    }
    
    fn error_expected(&self, expected: &str) -> ParseError {
        self.error(&format!("expected {}, found {:?}", expected, self.current.kind))
    }
}
```

### 10.2 Recovery Strategy

For initial implementation: stop at first error.

Future recovery points:
- Statement level: `;`, `}`
- Declaration level: `func`, `type`, `var`, `const`, `interface`

---

## 11. Testing

### 11.1 Test Files

Place `.gox` files in `tests/test_data/`:

```
crates/gox-syntax/tests/test_data/
├── hello.gox
├── fibonacci.gox
├── control_flow.gox
├── structs.gox
├── objects.gox
├── interfaces.gox
├── channels.gox
└── ...
```

### 11.2 Integration Tests

```rust
#[test]
fn test_parse_all_test_files() {
    let test_dir = Path::new("tests/test_data");
    for entry in fs::read_dir(test_dir).unwrap() {
        let path = entry.unwrap().path();
        if path.extension() == Some("gox".as_ref()) {
            let source = fs::read_to_string(&path).unwrap();
            let result = parser::parse(&source);
            assert!(result.is_ok(), "Failed to parse {:?}: {:?}", path, result);
        }
    }
}
```

---

## 12. Quick Reference: Spec to Parser Mapping

| Spec Section | Parser Component |
|--------------|------------------|
| §3 Lexical Structure | `lexer.rs`, `token.rs` |
| §4 Program Structure | `parse_file()`, `parse_top_decl()` |
| §5 Declarations | `parse_var_decl()`, `parse_const_decl()`, `parse_type_decl()` |
| §6 Types | `parse_type()` and type-specific parsers |
| §7 Interfaces/Methods | `parse_interface_decl()`, `parse_func_decl()` |
| §8 Statements | `parse_stmt()` and statement-specific parsers |
| §9 Expressions | `parse_expr()` (Pratt), `parse_primary()` |
| §10 Built-ins | Handled at analysis phase, not parsing |
| §11 Type Switches | `parse_type_switch_stmt()` |
